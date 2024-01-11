#ifndef __DTOFRAW_H__
#define __DTOFRAW_H__


typedef struct {
   short id;
   short slatid;
   short sector;
   short side;
   short panel;
   short slat;
   short cell[2];
   short qvc[2];
   short q1[2];
   short q2[2];
   short tvc[2];
   short t3[2];
   short t4[2];
} DTOFRAW_ST;
#endif /*__DTOFRAW_H__*/
