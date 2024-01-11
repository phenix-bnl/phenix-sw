#ifndef __MUNHITS_H__
#define __MUNHITS_H__


typedef struct {
   float tof;
   float de;
   float rhit[3];
   float phit[3];
   int track_num;
   short trk_id;
   short plane_num;
   short itrsub;
   short itrksub;
} MUNHITS_ST;
#endif /*__MUNHITS_H__*/
