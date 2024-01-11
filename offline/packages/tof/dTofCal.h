#ifndef __DTOFCAL_H__
#define __DTOFCAL_H__


typedef struct {
   short slatid;
   float qvc_corr[2];
   float qvc_corrlsr[2];
   float eloss_conv;
   float eloss_mip;
   float tvc_conv[2];
   float tvc_ped[2];
   float t0[2];
   float t0_lsr[2];
   float slew_a[2];
   float slew_b[2];
   float scint_vlight;
   float scint_attenu;
} DTOFCAL_ST;
#endif /*__DTOFCAL_H__*/
