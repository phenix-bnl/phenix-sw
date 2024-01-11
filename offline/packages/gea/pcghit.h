#ifndef __PCGHIT_H__
#define __PCGHIT_H__


typedef struct {
   float xyzinloc[3];
   float xyzoutloc[3];
   float xyzinglo[3];
   float tof;
   float dedx;
   float pathLength;
   short id;
   short arm;
   short sector;
   int mctrack;
} PCGHIT_ST;
#endif /*__PCGHIT_H__*/
