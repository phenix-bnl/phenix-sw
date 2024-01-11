#ifndef __DPADFASTSIMPAR_H__
#define __DPADFASTSIMPAR_H__


typedef struct {
   short pcnumber;
   int randseed[3];
   float efficiency[3];
   float phires[3];
   float zres[3];
   float phisep[3];
   float zsep[3];
   short verbose;
} DPADFASTSIMPAR_ST;
#endif /*__DPADFASTSIMPAR_H__*/
