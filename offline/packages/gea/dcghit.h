#ifndef __DCGHIT_H__
#define __DCGHIT_H__


typedef struct {
   float xyzinloc[3];
   float xyzoutloc[3];
   float xyzinglo[3];
   float tof;
   float pathLength;
   short id;
   short plane;
   short cell;
   short arm;
   int mctrack;
} DCGHIT_ST;
#endif /*__DCGHIT_H__*/
