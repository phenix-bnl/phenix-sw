#ifndef __DTOFUCAL_H__
#define __DTOFUCAL_H__


typedef struct {
   short slatid;
   float qvc_chgain[2];
   float tvc_conv[2];
   float tvc_ped[2];
   float slew_a[2];
   float slew_b[2];
   float scint_vlight;
   float scint_attenu;
   float tof_sigma;
} DTOFUCAL_ST;
#endif /*__DTOFUCAL_H__*/
