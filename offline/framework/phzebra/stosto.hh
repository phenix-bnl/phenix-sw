#ifndef __STOSTO_HH__
#define __STOSTO_HH__

#include "cfortran.h"

typedef struct {
  int ixstor_u;
  int ixstor_c;
  int ixstor_p;
  int ixstor_f;
} STOSTO_DEF;

#define Stosto COMMON_BLOCK(STOSTO,stosto)
COMMON_BLOCK_DEF(STOSTO_DEF,Stosto);

#endif /* __STOSTO_HH__ */
