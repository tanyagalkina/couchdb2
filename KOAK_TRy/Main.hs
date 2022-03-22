{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
    
import AST as K -- K for Kaleidoscope
import Utils
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.String
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import Foreign.Ptr
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Linking
import LLVM.Module
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import Numeric
import System.IO
import System.IO.Error
import Text.Read (readMaybe)
foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double
 
data JITEnv = JITEnv
 { jitEnvContext :: Context
 , jitEnvCompileLayer :: IRCompileLayer ObjectLinkingLayer
 , jitEnvModuleKey :: ModuleKey
 }

main::IO ()
main = do
    loadLibraryPermanently (Just "stdlib.dylib")
    withContext $ \ctx -> withHostTargetMachineDefault $ \tm ->
      withExecutionSession $ \exSession ->
        withSymbolResolver exSession (SymbolResolver symResolver) $ \symResolverPtr ->
          withObjectLinkingLayer exSession (const $ pure symResolverPtr) $ \linkingLayer ->
             withIRCompileLayer linkingLayer tm $ \compLayer ->
               withModuleKey exSession $ \mdlKey -> do
                 let env = JITEnv ctx compLayer mdlKey
                 _ast <- runReaderT (buildModuleT "main" repl) env
                 return ()
   
   -- This can eventually be used to resolve external functions, e.g. a stdlib call
symResolver::MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symResolver sym = do
  ptr <- getSymbolAddressInProcess sym
  putStrLn $ "Resolving " <> show sym <> " to 0x" <> showHex ptr ""
  return (Right (JITSymbol ptr defaultJITSymbolFlags))
   
repl :: ModuleBuilderT (ReaderT JITEnv IO) ()
repl = do
   liftIO $ hPutStr stderr "ready> "
   mline <- liftIO $ catchIOError (Just <$> getLine) eofHandler
   case mline of
     Nothing -> return ()
     Just l -> do
       case readMaybe l of
         Nothing ->  liftIO $ hPutStrLn stderr "Couldn't parse"
         Just ast -> do
           anon <- isAnonExpr <$> hoist (buildAST ast)
           def <- mostRecentDef
           
           llvmAst <- moduleSoFar "main"
           ctx <- lift $ asks jitEnvContext
           env <- lift ask
           liftIO $ withModuleFromAST ctx llvmAst $ \mdl -> do
             Text.hPutStrLn stderr $ ppll def
             let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
             -- this returns true if the module was modified
             withPassManager spec $ flip runPassManager mdl
             when anon (jit env mdl >>= hPrint stderr)
 
           when anon (removeDef def)
       repl
   where
     eofHandler e
       | isEOFError e = return Nothing
       | otherwise = ioError e
     isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
     isAnonExpr _ = False
     
jit :: JITEnv -> Module -> IO Double
jit JITEnv{jitEnvCompileLayer=compLayer, jitEnvModuleKey=mdlKey} mdl =
  withModule compLayer mdlKey mdl $ do
    mangled <- mangleSymbol compLayer "__anon_expr"
    Right (JITSymbol fPtr _) <- findSymbolIn compLayer mdlKey mangled False
    mkFun (castPtrToFunPtr (wordPtrToPtr fPtr))

type Binds = Map.Map String Operand

buildAST :: AST -> ModuleBuilder Operand
buildAST (Function (Prototype nameStr paramStrs) body) = do
  let n = fromString nameStr
  function n params Type.double $ \ops -> do
    let binds = Map.fromList (zip paramStrs ops)
    flip runReaderT binds $ buildExpr body >>= ret
  where params = zip (repeat Type.double) (map fromString paramStrs)

buildAST (Extern (Prototype nameStr params)) =
  extern (fromString nameStr) (replicate (length params) Type.double) Type.double

buildAST (TopLevelExpr x) = function "__anon_expr" [] Type.double $
  const $ flip runReaderT mempty $ buildExpr x >>= ret

buildExpr :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
buildExpr (Num x) = pure $ ConstantOperand (Float (Double x))
buildExpr (Var n) = do
  binds <- ask
  case binds Map.!? n of
    Just x -> pure x
    Nothing -> error $ "'" <> n <> "' doesn't exist in scope"

buildExpr (BinOp op a b) = do
  opA <- buildExpr a
  opB <- buildExpr b
  tmp <- instr opA opB
  if isCmp
    then uitofp tmp Type.double
    else return tmp
  where isCmp
          | Cmp _ <- op = True
          | otherwise = False
        instr = case op of
                  K.Add -> fadd
                  K.Sub -> fsub
                  K.Mul -> fmul
                  K.Cmp LT -> fcmp OLT
                  K.Cmp GT -> fcmp OGT
                  K.Cmp EQ -> fcmp OEQ

buildExpr (Call callee params) = do
  paramOps <- mapM buildExpr params
  let nam = fromString callee
      -- get a pointer to the function
      typ = FunctionType Type.double (replicate (length params) Type.double) False
      ptrTyp = Type.PointerType typ (AddrSpace 0)
      ref = GlobalReference ptrTyp nam
  call (ConstantOperand ref) (zip paramOps (repeat []))

buildExpr (If cond thenE elseE) = mdo
  _ifB <- block `named` "if"

  -- since everything is a double, false == 0
  let zero = ConstantOperand (Float (Double 0))
  condV <- buildExpr cond
  cmp <- fcmp ONE zero condV `named` "cmp"

  condBr cmp thenB elseB

  thenB <- block `named` "then"
  thenOp <- buildExpr thenE
  br mergeB

  elseB <- block `named` "else"
  elseOp <- buildExpr elseE
  br mergeB

  mergeB <- block `named` "ifcont"
  phi [(thenOp, thenB), (elseOp, elseB)]

buildExpr (For name init cond mStep body) = mdo
  preheaderB <- block `named` "preheader"

  initV <- buildExpr init `named` "init"
  
  -- build the condition expression with 'i' in the bindings
  initCondV <- withReaderT (Map.insert name initV) $
                (buildExpr cond >>= fcmp ONE zero) `named` "initcond"

  -- skip the loop if we don't meet the condition with the init
  condBr initCondV loopB afterB

  loopB <- block `named` "loop"
  i <- phi [(initV, preheaderB), (nextVar, loopB)] `named` "i"

  -- build the body expression with 'i' in the bindings
  withReaderT (Map.insert name i) $ buildExpr body `named` "body"

  -- default to 1 if there's no step defined
  stepV <- case mStep of
    Just step -> buildExpr step
    Nothing -> return $ ConstantOperand (Float (Double 1))

  nextVar <- fadd i stepV `named` "nextvar"

  let zero = ConstantOperand (Float (Double 0))
  -- again we need 'i' in the bindings
  condV <- withReaderT (Map.insert name i) $
            (buildExpr cond >>= fcmp ONE zero) `named` "cond"
  condBr condV loopB afterB

  afterB <- block `named` "after"
  -- since a for loop doesn't really have a value, return 0
  return $ ConstantOperand (Float (Double 0))
