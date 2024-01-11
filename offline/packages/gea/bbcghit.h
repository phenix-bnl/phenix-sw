#ifndef __BBCGHIT_H__
#define __BBCGHIT_H__


typedef struct {
   float pos[3];
   float mom[3];
   float del;
   float tof;
   float len;
   short pmt;
   short pid;
   int mctrack;
} BBCGHIT_ST;
#endif /*__BBCGHIT_H__*/
