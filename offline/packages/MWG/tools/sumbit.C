#include "Tools.h"
#include <cmath>

Float_t Tools::sumbit(Float_t hitbit)
{
   Float_t sum = 0.;
   Int_t n;
   n = (Int_t) hitbit;

   for (int i=0; i<16;i++) {
     if(n & (1<<i)) sum += 1;
   }
   return sum;
}

