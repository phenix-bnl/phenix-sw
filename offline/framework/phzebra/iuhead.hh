//#include "cfortran.h"

typedef struct {
  int iuhead[100];
} IUHEAD_DEF;
//#define Iuhead COMMON_BLOCK(IUHEAD,iuhead)
//COMMON_BLOCK_DEF(IUHEAD_DEF,Iuhead);

extern IUHEAD_DEF *Iuhead;
