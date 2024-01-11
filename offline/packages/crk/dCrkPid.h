#ifndef __DCRKPID_H__
#define __DCRKPID_H__


typedef struct {
   short id;
   short proj_id;
   short faccept;
   short npmt;
   float npe;
   float timing;
   float chi2;
   float rdisp;
   float Lpath;
   float xproj[3];
   float chi2b;
   float dt;
} DCRKPID_ST;
#endif /*__DCRKPID_H__*/
