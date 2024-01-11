#ifndef __DPADFEM_H__
#define __DPADFEM_H__


typedef struct {
   int CAV1;
   int det;
   int Ecounter;
   int adr;
   int Flag;
   int Bcounter;
   int Word[117];
   int usr1;
   int usr2;
   int usr3;
   int usr4;
   int usr5;
   int usr6;
   int usr7;
   int usr8;
   int parity;
   int CAV2;
} DPADFEM_ST;
#endif /*__DPADFEM_H__*/