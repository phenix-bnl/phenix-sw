#ifndef __DDCHHIT_H__
#define __DDCHHIT_H__

typedef struct {
   short id;
   short arm;
   short plane;
   short cell;
   short side;
   float distance;
   float width;
   short idraw1;
   short idraw2;
   short idmirror;
   int time1;
   int time2;
   short used;
   float xyz[3];
   float err[3];
   float vxyz[3];
} DDCHHIT_ST;
#endif /*__DDCHHIT_H__*/
