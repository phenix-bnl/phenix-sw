#ifndef __DPADGEOM_H__
#define __DPADGEOM_H__


typedef struct {
   float pdxoff[3];
   float pdzoff[3];
   float pdgas[3];
   float aasep[3];
   float pxlen[3];
   float wside[3];
   float wcent[3];
   float pxsep[3];
   float clsep[3];
   short npdsec[3];
   short npdwr[3];
   short npdx[3];
   short npdz[3];
   short sectperarm[3];
   float inradius[3];
   float zgap[3];
   float phibote;
   float phitope;
   float phibotw;
   float phitopw;
} DPADGEOM_ST;
#endif /*__DPADGEOM_H__*/
