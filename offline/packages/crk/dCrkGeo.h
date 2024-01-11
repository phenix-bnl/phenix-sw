#ifndef __DCRKGEO_H__
#define __DCRKGEO_H__


typedef struct {
   float phi_cntr;
   float phi_open;
   float dphi_carm;
   float dphi_cshe;
   float pmt_phi_min;
   float pmt_phi_max;
   float pmt_dphi;
   float r_pmt_ent;
   float dx_pmt[32];
   float r_pmt[32];
   float z_pmt[32];
   float theta_pmt[32];
   float mir_rin;
   float mir_thck;
   float mir_theta1;
   float mir_theta2;
   float mir_thetacut;
   float mir_phi1;
   float mir_phi2;
   float mir_dz;
   float wi1_rin;
   float wi1_thck;
   float wi1_zend;
   float wi2_rin;
   float wi2_thck;
   float wi2_zend;
} DCRKGEO_ST;
#endif /*__DCRKGEO_H__*/
