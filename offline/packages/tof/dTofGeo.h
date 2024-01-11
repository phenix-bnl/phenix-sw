#ifndef __DTOFGEO_H__
#define __DTOFGEO_H__


typedef struct {
   short slatid;
   short sector;
   short side;
   short panel;
   short slat;
   float pos[3];
   float r;
   float phi;
} DTOFGEO_ST;
#endif /*__DTOFGEO_H__*/
