#ifndef __GCFLAG_HH__
#define __GCFLAG_HH__

#include "cfortran.h"

typedef struct {
  int idebug;
  int idemin;
  int idemax;
  int itest;
  int idrun;
  int idevt;
  int ieorun ;
  int ieotri;
  int ievent;
  int iswit[10];
  int ifinit[20];
  int nevent;
  int nrndm[2];
} GCFLAG_DEF;

#define Gcflag COMMON_BLOCK(GCFLAG,gcflag)
COMMON_BLOCK_DEF(GCFLAG_DEF,Gcflag);

#endif /* __GCFLAG_HH__ */
