#ifndef __DTOFPID_H__
#define __DTOFPID_H__


typedef struct {
   short id;
   short id_assoc;
   short id_cgl;
   short id_cgt;
   short id_tof;
   short pid;
   unsigned short pid_stat;
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
} DTOFPID_ST;
#endif /*__DTOFPID_H__*/
