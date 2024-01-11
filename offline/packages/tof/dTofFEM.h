#ifndef __DTOFFEM_H__
#define __DTOFFEM_H__


typedef struct {
   unsigned short CAV1;
   unsigned short det;
   unsigned short Ecounter;
   unsigned short adr;
   unsigned short Flag;
   unsigned short Bcounter;
   unsigned short Word[1136];
   unsigned short parity;
   unsigned short CAV2;
} DTOFFEM_ST;
#endif /*__DTOFFEM_H__*/
