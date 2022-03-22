#include <stdio.h>
   // Takes a double and writes it to stdout
   double putchard(double x) {
           int res = putchar((int)x);
          fflush(stdout);
          return (double)res;
   }