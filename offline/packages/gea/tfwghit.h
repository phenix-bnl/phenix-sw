#ifndef __TFWGHIT_H__
#define __TFWGHIT_H__


typedef struct {
   float xyzinglo[3];
   float xyzinloc[3];
   float xyzoutloc[3];
   float tof;
   float pathLength;
   float dele;
   int panel;
   int idpart;
   int mctrack;
   int nFile;
   int track;      // GEANT's track-in-subevent number
   int isubevent;
} TFWGHIT_ST;
#endif /*__TFWGHIT_H__*/
