#ifndef __DPADSLOWSIMPAR_H__
#define __DPADSLOWSIMPAR_H__


typedef struct {
   short verbose;
   short pcnumber;
   int randseed[3];
   float qnoise[3];
   float threshold[3];
} DPADSLOWSIMPAR_ST;
#endif /*__DPADSLOWSIMPAR_H__*/
