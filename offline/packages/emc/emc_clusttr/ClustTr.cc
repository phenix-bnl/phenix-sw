
#include "ClustTr.hh"

ClassImp(ClustTr)

ClustTr::ClustTr(){

};
void ClustTr::Reset(){
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
  i = 3;
  while( i-- ){
    pos[i] = 0;
  }
  i = 2;
  while( i-- ){
    disp[i] = 0;
    padisp[i] = 0;
    ind[i] = 0;
  }

  // Tracking information
  inters = 0;
  ptot = 0;
  cglarm = 0;
  quality = 0;
  drmin = 0;
  pathl = 0;
  crk_acc = 0;
  crk_npmt0 = 0;
  crk_npmt1 = 0;
  crk_npe0 = 0;
  crk_npe1 = 0;
  crk_chi2 = 0;
  crk_disp = 0;
  i = 3;
  while( i-- ){
    proj[i] = 0;
    dir[i] = 0;
    newproj[i] = 0;
    corpos[i] = 0;
  }

  // Tracking information after Z-flip
  inters_s = 0;
  ptot_s = 0;
  cglarm_s = 0;
  quality_s = 0;
  drmin_s = 0;
  pathl_s = 0;
  crk_acc_s = 0;
  crk_npmt0_s = 0;
  crk_npmt1_s = 0;
  crk_npe0_s = 0;
  crk_npe1_s = 0;
  crk_chi2_s = 0;
  crk_disp_s = 0;
  i = 3;
  while( i-- ){
    proj_s[i] = 0;
    dir_s[i] = 0;
    newproj_s[i] = 0;
    corpos_s[i] = 0;
  }


};
//

