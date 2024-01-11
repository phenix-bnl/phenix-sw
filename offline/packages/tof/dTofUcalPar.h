#ifndef __DTOFUCALPAR_H__
#define __DTOFUCALPAR_H__


typedef struct {
   short option;
   float qvc_chgain[2];
   float tvc_conv[2];
   float tvc_ped[2];
   float slew_a[2];
   float slew_b[2];
   float scint_vlight;
   float scint_attenu;
   float tof_sigma;
   char datafile[30];
} DTOFUCALPAR_ST;
#endif /*__DTOFUCALPAR_H__*/
