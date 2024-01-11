#ifndef __DZDCOUT_H__
#define __DZDCOUT_H__


typedef struct {
   float Charge[8];
   float Timing0[8];
   float Timing1[8];
   float Energy[2];
   float Timing[2];
   float Zvertex;
   float ZvertexError;
   float TimeZero;
   float timeZeroError;
} DZDCOUT_ST;
#endif /*__DZDCOUT_H__*/
