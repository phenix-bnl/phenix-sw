#ifndef __HEADER_H__
#define __HEADER_H__


typedef struct {
   short run;
   short event;
   short multiplicity;
   float b;
   short a1;
   short z1;
   short a2;
   short z2;
   float sqrt_s;
   float bmin;
   float bmax;
   float t0femto;
   float vertex[3];
   short itime;
   int idate;
   int nrndm[2];
   short isqStart;
   int iSeconds;
   int maxTrueTrack;
} HEADER_ST;
#endif /*__HEADER_H__*/
