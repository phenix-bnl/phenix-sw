#ifndef __GCBANK_HH__
#define __GCBANK_HH__

#include "cfortran.h"

#include "pisasize.h"

const int NWGEAN = NWORDS_GCBANK;

typedef struct GCBANK_DEF {
  int nzebra;
  int gversn;
  int zversn;
  int ixstor;
  int ixdiv;
  int ixcons;
  int fence[16]; 
  int lq[8];
  union {
   int   iq[NWORDS_GCBANK];
   float  q[NWORDS_GCBANK];
  };
} GCBANK_DEF;

#define Gcbank COMMON_BLOCK(GCBANK,gcbank)
COMMON_BLOCK_DEF(GCBANK_DEF,Gcbank);

#endif /* __GCBANK_HH__ */
