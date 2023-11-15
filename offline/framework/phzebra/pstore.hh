#ifndef __PSTORE_HH__
#define __PSTORE_HH__

#include "cfortran.h"

#ifndef PF_STORE_SIZE
#define PF_STORE_SIZE 6500000
#endif 

const int PF_STORE_DIV12 = 10000;
const int PF_PDIV_MAX = 400000;
const int PF_EDIV_MAX = 5500000;

typedef struct {
  int ixdiv_fp;
  int ixdiv_fe;
  int fence[10];
  union {
    int Lqf[PF_STORE_SIZE];
    struct {
      int Dummy[8];
      union {
	float Qf[2];
	int Iqf[2];
      };
    } s1;
    struct {
      int Lsafe[10];
      int Lstruc;
      int Lrefer;
      int Div12[PF_STORE_DIV12];
    } s2;
  };
} PSTORE_DEF;

#define lqf Lqf
#define qf  s1.Qf
#define iqf s1.Iqf
#define lsafe s2.Lsafe
#define lstruc s2.Lstruc
#define lrefer s2.Lrefer
#define div12 s2.Div12

#define Pstore COMMON_BLOCK(PSTORE,pstore)
COMMON_BLOCK_DEF(PSTORE_DEF,Pstore);

#endif /* __PSTORE_HH__ */

