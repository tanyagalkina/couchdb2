module AST where
 
 import Data.Char
 import Text.Read 
 import Text.ParserCombinators.ReadP hiding ((+++), (<++), choice)
 
 data Expr = Num Double
           | Var String
           | BinOp BinOp Expr Expr
           | Call String [Expr]
           | If Expr Expr Expr
           | For String Expr Expr (Maybe Expr) Expr
   deriving Show
 
 data BinOp = Add | Sub | Mul | Cmp Ordering
   deriving Show
 
 instance Read Expr where
   readPrec = parens $ choice [ parseNum
                              , parseVar
                              , parseCall
                              , parseIf
                              , parseFor
                              , parseBinOp "<" 10 (Cmp LT)
                              , parseBinOp ">" 10 (Cmp GT)
                              , parseBinOp "==" 10 (Cmp EQ)
                              , parseBinOp "+" 20 Add
                              , parseBinOp "-" 20 Sub
                              , parseBinOp "*" 40 Mul
                              ]
     where parseNum = Num <$> readPrec
           parseVar = Var <$> lift (munch1 isAlpha)
           parseBinOp s prc op = prec prc $ do
             a <- step readPrec
             spaced $ string s
             b <- readPrec
             return (BinOp op a b)
           parseCall = do
             func <- lift (munch1 isAlpha)
             params <- lift $ between (char '(') (char ')') $
                         sepBy (readS_to_P reads)
                               (skipSpaces >> char ',' >> skipSpaces)
             return (Call func params)
           parseIf = do
             spaced $ string "if" 
             cond <- readPrec
             spaced $ string "then"
             thenE <- readPrec
             spaced $ string "else"
             elseE <- readPrec
             return (If cond thenE elseE)
           parseFor = do
             spaced $ string "for"
             identifier <- lift (munch1 isAlpha)
             spaced $ char '='
             start <- readPrec
             spaced $ char ','
             cond <- readPrec
             stp <- (spaced (char ',') >> Just <$> step readPrec)
                      <++ pure Nothing
             spaced $ string "in"
             body <- readPrec
             return (For identifier start cond stp body)
           spaced f = lift $ skipSpaces >> f >> skipSpaces
             
 data Prototype = Prototype String [String]
   deriving Show
 
 instance Read Prototype where
   readPrec = lift $ do
     name <- munch1 isAlpha
     params <- between (char '(') (char ')') $
                 sepBy (munch1 isAlpha) skipSpaces
     return (Prototype name params)
 
 data AST = Function Prototype Expr
          | Extern Prototype
          | TopLevelExpr Expr
   deriving Show
 
 instance Read AST where
   readPrec = parseFunction +++ parseExtern +++ parseTopLevel
     where parseFunction = do
             lift $ string "def" >> skipSpaces
             Function <$> readPrec <*> readPrec
           parseExtern = do
             lift $ string "extern" >> skipSpaces
             Extern <$> readPrec
           parseTopLevel = TopLevelExpr <$> readPrec