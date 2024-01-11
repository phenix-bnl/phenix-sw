#ifndef __DTOFPIDPAR_H__
#define __DTOFPIDPAR_H__


typedef struct {
   short pid;
   float m2mean;
   float cm2[3];
   float m2min;
   float m2max;
   float pmin;
   float pmax;
   short charge;
   float factor;
} DTOFPIDPAR_ST;
#endif /*__DTOFPIDPAR_H__*/
