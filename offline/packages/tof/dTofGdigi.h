#ifndef __DTOFGDIGI_H__
#define __DTOFGDIGI_H__


typedef struct {
   short id;
   short slatid;
   short panel;
   short column;
   short pslat;
   short slat_seq;
   int mctrack;
   short partl;
   float tof;
   float eloss;
   float pos_m[3];
   float pos_hit_slat;
   float theta;
   float phi;
   float p_m[3];
   float path;
   short nslathit;
   short hits_seq;
} DTOFGDIGI_ST;
#endif /*__DTOFGDIGI_H__*/
