#ifndef __CRKGHIT_H__
#define __CRKGHIT_H__


typedef struct {
   float x;
   float y;
   float z;
   float px;
   float py;
   float pz;
   float tof;
   float bp1;
   float bp2;
   short pid;
   int parent;
   short pmt;
   short tra;
   short nbf;
   short bi1;
   short bi2;
} CRKGHIT_ST;
#endif /*__CRKGHIT_H__*/
