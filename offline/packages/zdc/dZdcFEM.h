#ifndef __DZDCFEM_H__
#define __DZDCFEM_H__


typedef struct {
   unsigned short CAV1;
   unsigned short det;
   unsigned short Ecounter;
   unsigned short adr;
   unsigned short Flag;
   unsigned short Bcounter;
   unsigned short Word[25];
   unsigned short parity;
   unsigned short CAV2;
} DZDCFEM_ST;
#endif /*__DZDCFEM_H__*/
