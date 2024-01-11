#ifndef __DTECGHITDCM_H__
#define __DTECGHITDCM_H__


typedef struct {
   short ghitid;
   short arm;
   short sector;
   short side;
   short wire;
   short crate;
   short slot;
   short channel;
   short binnum;
   float fraction;
} DTECGHITDCM_ST;
#endif /*__DTECGHITDCM_H__*/
