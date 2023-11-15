#ifndef __PAWC_HH__
#define __PAWC_HH__

#include "cfortran.h"

#ifndef NWORDS_PAWC
#define NWORDS_PAWC 1000000 
#endif
const int NWPAW = NWORDS_PAWC;

typedef struct {
  int paw[NWORDS_PAWC];
} PAWC_DEF;

#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

#endif /* __PAWC_HH__ */
