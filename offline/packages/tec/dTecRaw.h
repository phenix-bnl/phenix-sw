#ifndef __DTECRAW_H__
#define __DTECRAW_H__


typedef struct {
   short id;
   short arm;
   short plane;
   short sector;
   short side;
   short wire;
   short amplitude[80];
} DTECRAW_ST;
#endif /*__DTECRAW_H__*/
