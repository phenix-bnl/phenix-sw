
#include "TrackCl.hh"

ClassImp(TrackCl)

TrackCl::TrackCl(){

};
void TrackCl::Reset(){
  int i;
  // Global information
  run = 0;
  evn = 0;
  zdcz = 0;
  zdct0 = 0;
  zdcch = 0;
  bbcz = 0;
  bbct0 = 0;
  bbcnhit = 0;
  bbcch = 0;
  pc1nhit = 0;
  emcnhit = 0;
  // Tracking information
  inters = 0;
  ptot = 0;
  cglarm = 0;
  quality = 0;
  pathl = 0;
  alpha = 0;
  beta = 0;
  i = 3;
  while( i-- ){
    proj[i] = 0;
    dir[i] = 0;
  }
  // EMCal Clustering information
  arm = 0;
  sector = 0;
  twrhit = 0;
  clustno = 0;
  nsh = 0;
  tof = 0;
  e = 0;
  ecent = 0;
  ecore = 0;
  ecorr = 0;
  tofcorr = 0;
  chi2 = 0;
  drmin = 0;
  corpathl = 0;
  // EMCal Clustering information after Z-flip
  arm_s = 0;
  sector_s = 0;
  twrhit_s = 0;
  clustno_s = 0;
  nsh_s = 0;
  tof_s = 0;
  e_s = 0;
  ecent_s = 0;
  ecore_s = 0;
  ecorr_s = 0;
  tofcorr_s = 0;
  chi2_s = 0;
  drmin_s = 0;
  corpathl_s = 0;
  i = 3;
  while( i-- ){
    pos[i] = 0;
    pos_s[i] = 0;
    newproj[i] = 0;
    newproj_s[i] = 0;
    corpos[i] = 0;
    corpos_s[i] = 0;
  }
  i = 2;
  while( i-- ){
    disp[i] = 0;
    disp_s[i] = 0;
    padisp[i] = 0;
    padisp_s[i] = 0;
    ind[i] = 0;
    ind_s[i] = 0;
  }
  
  // RICH information
  crk_acc = 0 ;
  crk_npmt0 = 0;
  crk_npmt1 = 0;
  crk_npe0 = 0;
  crk_npe1 = 0;
  crk_chi2 = 0;
  crk_disp = 0;

  // RICH information after Z-flip
  crk_acc_s = 0 ;
  crk_npmt0_s = 0;
  crk_npmt1_s = 0;
  crk_npe0_s = 0;
  crk_npe1_s = 0;
  crk_chi2_s = 0;
  crk_disp_s = 0;

};
//

