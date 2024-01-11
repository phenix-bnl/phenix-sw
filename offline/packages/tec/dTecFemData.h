#ifndef __DTECFEMDATA_H__
#define __DTECFEMDATA_H__


typedef struct {
   short id;
   short crate;
   short slot;
   short psadd;
   short ichan;
   short amplitude[80];
} DTECFEMDATA_ST;
#endif /*__DTECFEMDATA_H__*/
