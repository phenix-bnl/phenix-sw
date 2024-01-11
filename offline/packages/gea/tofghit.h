#ifndef __TOFGHIT_H__
#define __TOFGHIT_H__


typedef struct {
   float pos_m[3];
   float pos_hit_slat;
   float p_m[3];
   float tof;
   float dele;
   short subvol;
   short panel;
   short column;
   short pslat;
   short slat_seq;
   short partl;
   int mctrack;
} TOFGHIT_ST;
#endif /*__TOFGHIT_H__*/
