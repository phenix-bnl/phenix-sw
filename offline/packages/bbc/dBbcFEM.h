#ifndef __DBBCFEM_H__
#define __DBBCFEM_H__


typedef struct {
   unsigned short CAV1;
   unsigned short det;
   unsigned short Ecounter;
   unsigned short adr;
   unsigned short Flag;
   unsigned short Bcounter;
   unsigned short Word[400];
   unsigned short parity;
   unsigned short CAV2;
} DBBCFEM_ST;
#endif /*__DBBCFEM_H__*/
