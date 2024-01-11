#ifndef UTIMATCH_H
#define UTIMATCH_H

class utiMatch {
public:
  utiMatch();
  //functions for calculating broad and narrow sigmas, this is west arm only!!
  void  pc2pc3_corr_phi(float inpc2sdphi,float inpc3sdphi,float&outnaphi,float&outbrphi);
  void  pc2pc3_corr_z(float inpc2sdz,float inpc3sdz,float&outnaz,float&outbrz,float centclock);

  //two functions that akiba requested:
  float PBSC_sdz_electron(float sdz0,float p, float beta);
  float PBGL_sdz_electron(float sdz0,float p, float beta);
  float PBSC_sdphi_electron(float sdphi0,float p);
  float PBGL_sdphi_electron(float sdphi0,float p);

  //functions to calculate the matching in number if sigmas
  

  //run3 functions
  float d_PC2_phi_match(const float p, const float zed, const float dv);
  float d_PC2_z_match(const float p, const float zed, const float dv);
  float d_PC3e_phi_match(const float p, const float zed, const float dv);
  float d_PC3e_z_match(const float p, const float zed, const float dv);
  float d_PC3w_phi_match(const float p, const float zed, const float dv);
  float d_PC3w_z_match(const float p, const float zed, const float dv);
  float d_TOF_phi_match(const float p, const float zed, const float dv);
  float d_TOF_z_match(const float p, const float zed, const float dv);
  //run2 functions
  float d_PC2_phi_match(const float p, const float dv);
  float d_PC2_z_match(const float p, const float dv);
  float d_PC3e_phi_match(const float p, const float dv);
  float d_PC3e_z_match(const float p, const float dv);
  float d_PC3w_phi_match(const float p, const float dv);
  float d_PC3w_z_match(const float p, const float dv);
  float d_TOF_phi_match(const float p, const float dv);
  float d_TOF_z_match(const float p, const float dv);
  //run2 and run3 functions
  float d_TEC_phi_match(const float p, const float dv);
  float d_TEC_alpha_match(const float p, const float dv);
  float d_PBGL_phi_match(const float p, const float dv,const int type=3);
  float d_PBGL_z_match(const float p, const float beta,const float dv,const int type=3);
  float d_PBSCe_phi_match(const float p, const float dv,const int type=3);
  float d_PBSCe_z_match(const float p,const float beta,const float dv,const int type=3);
  float d_PBSCw_phi_match(const float p, const float dv,const int type=3);
  float d_PBSCw_z_match(const float p,const float beta,const float dv,const int type=3);
  //functions to calculate the dv from number of sigma
  //run3 functions
  float dv_PC2_phi_match(const float p, const float zed, const float sig);
  float dv_PC2_z_match(const float p, const float zed, const float sig);
  float dv_PC3e_phi_match(const float p, const float zed, const float sig);
  float dv_PC3e_z_match(const float p, const float zed, const float sig);
  float dv_PC3w_phi_match(const float p, const float zed, const float sig);
  float dv_PC3w_z_match(const float p, const float zed, const float sig);
  float dv_TOF_phi_match(const float p, const float zed, const float sig);
  float dv_TOF_z_match(const float p, const float zed, const float sig);
  //run2 functions
  float dv_PC2_phi_match(const float p, const float sig);
  float dv_PC2_z_match(const float p, const float sig);
  float dv_PC3e_phi_match(const float p, const float sig);
  float dv_PC3e_z_match(const float p, const float sig);
  float dv_PC3w_phi_match(const float p, const float sig);
  float dv_PC3w_z_match(const float p, const float sig);
  float dv_TOF_phi_match(const float p, const float sig);
  float dv_TOF_z_match(const float p, const float sig);
  //run2 and run3 functions
  float dv_TEC_phi_match(const float p, const float sig);
  float dv_TEC_alpha_match(const float p, const float sig);
  float dv_PBGL_phi_match(const float p, const float sig,const int type=3);
  float dv_PBGL_z_match(const float p, const float beta,const float sig,const int type=3);
  float dv_PBSCe_phi_match(const float p, const float sig,const int type=3);
  float dv_PBSCe_z_match(const float p,const float beta,const float sig,const int type=3);
  float dv_PBSCw_phi_match(const float p, const float sig,const int type=3);
  float dv_PBSCw_z_match(const float p,const float beta,const float sig,const int type=3);
  //
  // functions to set the matching parameters:
  //
  //run2 functions
  void set_tec_alpha_match(float mplus1,float mplus2,float mminus1,
                           float mminus2,float sig1,float sig2);
  void set_xxx_z_match  (int detector,float m1,float m2,float sig1,float sig2);
  void set_xxx_phi_match(int detector,
			 float mplus1,float mplus2,float mminus1,float mminus2,
			 float sig1,float sig2);
  void set_xxx_pbglz_shift (float gamma1,float gamma2,
			    float mip1,float mip2,float nucl1,float nucl2);
  void set_xxx_pbscez_shift(float gamma1,float gamma2,
			    float mip1,float mip2,float nucl1,float nucl2);
  void set_xxx_pbscwz_shift(float electron1,float electron2,
			    float mip1,float mip2,float nucl1,float nucl2);
  //
  // call following functions if you want to get the shift that needed for the correction.
  //
  //run3 functions
  float dr_PC2_phi_match(const float p, const float zed, const float dv,float &shift)  {
    float sig = d_PC2_phi_match(p,zed,dv); 
    shift = tmpshift;
    return sig;
  }
  float dr_PC2_z_match(const float p, const float zed, const float dv,float &shift)    {
    float sig = d_PC2_z_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3e_phi_match(const float p, const float zed, const float dv,float &shift) {
    float sig = d_PC3e_phi_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3e_z_match(const float p, const float zed, const float dv,float &shift)   {
    float sig = d_PC3e_z_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3w_phi_match(const float p, const float zed, const float dv,float &shift) {
    float sig = d_PC3w_phi_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3w_z_match(const float p, const float zed, const float dv,float &shift)   {
    float sig = d_PC3w_z_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_TOF_phi_match(const float p, const float zed, const float dv,float &shift)  {
    float sig = d_TOF_phi_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_TOF_z_match(const float p, const float zed, const float dv,float &shift)    {
    float sig = d_TOF_z_match(p,zed,dv);
    shift = tmpshift;
    return sig;
  }
  //run2 functions
  float dr_PC2_phi_match(const float p, const float dv,float &shift)  {
    float sig = d_PC2_phi_match(p,dv); 
    shift = tmpshift;
    return sig;
  }
  float dr_PC2_z_match(const float p, const float dv,float &shift)    {
    float sig = d_PC2_z_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3e_phi_match(const float p, const float dv,float &shift) {
    float sig = d_PC3e_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3e_z_match(const float p, const float dv,float &shift)   {
    float sig = d_PC3e_z_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3w_phi_match(const float p, const float dv,float &shift) {
    float sig = d_PC3w_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PC3w_z_match(const float p, const float dv,float &shift)   {
    float sig = d_PC3w_z_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_TOF_phi_match(const float p, const float dv,float &shift)  {
    float sig = d_TOF_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_TOF_z_match(const float p, const float dv,float &shift)    {
    float sig = d_TOF_z_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  //run2 and run3 functions
  float dr_TEC_phi_match(const float p, const float dv,float &shift)  {
    float sig = d_TEC_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_TEC_alpha_match(const float p, const float dv,float &shift)    {
    float sig = d_TEC_alpha_match(p,dv);
    shift = tmpshift;
    return sig;
  }

  float dr_PBGL_phi_match(const float p, const float dv,float &shift) {
    float sig = d_PBGL_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PBGL_z_match(const float p, const float beta,const float dv,const int type,float &shift){
    float sig = d_PBGL_z_match(p,beta,dv,type);
    shift = tmpshift;
    return sig;
  }
  float dr_PBSCe_phi_match(const float p, const float dv,float &shift){
    float sig = d_PBSCe_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PBSCe_z_match(const float p,const float beta,const float dv,const int type,float &shift){
    float sig = d_PBSCe_z_match(p,beta,dv,type);
    shift = tmpshift;
    return sig;
  }
  float dr_PBSCw_phi_match(const float p, const float dv,float &shift){
    float sig = d_PBSCw_phi_match(p,dv);
    shift = tmpshift;
    return sig;
  }
  float dr_PBSCw_z_match(const float p,const float beta,const float dv,const int type,float &shift){
    float sig = d_PBSCw_z_match(p,beta,dv,type);
    shift = tmpshift;
    return sig;
  }
private:
  static char subsystems[8][100];
  void  Load_default();
  float tmpshift;
  //
  // correction parameters for dv
  //
  //run3 variables
  float pc2phi_mean_run3[4][2],         pc2phi_rms_run3[2];
  float pc2z_mean_run3[2][2],           pc2z_rms_run3[2];
  float pc3ephi_mean_run3[4][2],        pc3ephi_rms_run3[2];
  float pc3ez_mean_run3[2][2],          pc3ez_rms_run3[2];
  float pc3wphi_mean_run3[4][2],        pc3wphi_rms_run3[2];
  float pc3wz_mean_run3[2][2],          pc3wz_rms_run3[2];
  float tofphi_mean_run3[2][2],         tofphi_rms_run3[2];
  float tofz_mean_run3[2][2],           tofz_rms_run3[2];

  //run2 variables
  float pc2phi_mean_plus[2],       pc2phi_mean_minus[2],          pc2phi_rms[2];
  float pc2z_mean[2],              pc2z_rms[2];

  float pc3ephi_mean_plus[2],      pc3ephi_mean_minus[2],         pc3ephi_rms[2];
  float pc3ez_mean[2],             pc3ez_rms[2];

  float pc3wphi_mean_plus[2],      pc3wphi_mean_minus[2],         pc3wphi_rms[2];
  float pc3wz_mean[2],             pc3wz_rms[2];

  float tofphi_mean_plus[2],       tofphi_mean_minus[2],          tofphi_rms[2];
  float tofz_mean[2],              tofz_rms[2];

  //run2 and run3 variables
  float tecphi_mean_plus[2],       tecphi_mean_minus[2],          tecphi_rms[2];
  float tecalpha_mean_plus[2],     tecalpha_mean_minus[2],         tecalpha_rms[2];

  float pbglphi_mean_plus[2],      pbglphi_mean_minus[2],          pbglphi_rms[2];
  float pbglz_mean[2],             pbglz_rms[2];
  float pbglz_shift_electron[2],   pbglz_shift_mip[2],             pbglz_shift_nucl[2];

  float pbscephi_mean_plus[2],     pbscephi_mean_minus[2],         pbscephi_rms[2];
  float pbscez_mean[2],            pbscez_rms[2];
  float pbscez_shift_electron[2],  pbscez_shift_mip[2],            pbscez_shift_nucl[2];

  float pbscwphi_mean_plus[2],     pbscwphi_mean_minus[2],         pbscwphi_rms[2];
  float pbscwz_mean[2],            pbscwz_rms[2];
  float pbscwz_shift_electron[2],  pbscwz_shift_mip[2],            pbscwz_shift_nucl[2];



};

#endif



