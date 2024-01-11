#ifndef __DTOFASSOCIATE_H__
#define __DTOFASSOCIATE_H__


typedef struct {
   short id;
   short id_cgl;
   short id_cgt;
   short id_dch;
   short id_tec;
   short id_tof;
   unsigned short asc_stat;
   short slatid;
   float tof;
   float eloss;
   float xtof[3];
   float xtrk[3];
   float vtrk[3];
   unsigned short tof_stat;
   float path;
   float beta;
   float m2;
   short charge;
   float pxyz[3];
   float dpxyz[3];
   float zvertex;
   float dzvertex;
   float phi;
   float dphi;
   float theta;
   float dtheta;
   float quality;
   float momrec_ndist[3];
} DTOFASSOCIATE_ST;
#endif /*__DTOFASSOCIATE_H__*/
