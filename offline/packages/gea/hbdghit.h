#ifndef __HBDGHIT_H__
#define __HBDGHIT_H__


typedef struct {
   float xyzinloc[3];
   float xyzoutloc[3];
   float tof;
   short id;
   short sector;
   short detflag;
   int idpart;
   int mctrack;
} HBDGHIT_ST;
#endif /*__HBDGHIT_H__*/
