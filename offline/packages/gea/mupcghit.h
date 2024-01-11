#ifndef __MUPCGHIT_H__
#define __MUPCGHIT_H__


typedef struct {
   float xyzinloc[3];
   float xyzoutloc[3];
   float xyzinglo[3];
   float tof;
   float dedx;
   float pathLength;
   short id;
   short arm;
   int mctrack;
} MUPCGHIT_ST;
#endif /*__MUPCGHIT_H__*/
