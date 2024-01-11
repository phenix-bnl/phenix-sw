#ifndef __DEMCGEOMETRY_H__
#define __DEMCGEOMETRY_H__


typedef struct {
   short id;
   short hwkey;
   short swkey;
   short arm;
   short sector;
   short ind[2];
   float nomxyz[3];
   float actxyz[3];
   float nomunitv[3];
   float actunitv[3];
   float nomtheta;
   float nomphi;
   float acttheta;
   float actphi;
   float nomdist;
   float actdist;
   float nomflash;
   float actflash;
   float sectheta;
   float secphi;
} DEMCGEOMETRY_ST;
#endif /*__DEMCGEOMETRY_H__*/
