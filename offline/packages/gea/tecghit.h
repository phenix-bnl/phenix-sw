#ifndef __TECGHIT_H__
#define __TECGHIT_H__


typedef struct {
   float xyzinloc[3];
   float xyzoutloc[3];
   float xyzinglo[3];
   float tof;
   float dedx;
   short id;
   short arm;
   short plane;
   short sector;
   int mctrack;
} TECGHIT_ST;
#endif /*__TECGHIT_H__*/
