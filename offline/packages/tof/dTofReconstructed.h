#ifndef __DTOFRECONSTRUCTED_H__
#define __DTOFRECONSTRUCTED_H__


typedef struct {
   short id;
   short slatid;
   short sector;
   short side;
   short panel;
   short slat;
   float tof;
   float tof_err;
   float eloss;
   float eloss_err;
   float xtof[3];
   float xtof_err[3];
   short qvc[2];
   short tvc[2];
   float tdiff;
} DTOFRECONSTRUCTED_ST;
#endif /*__DTOFRECONSTRUCTED_H__*/
