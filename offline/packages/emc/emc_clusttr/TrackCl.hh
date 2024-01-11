#ifndef TrackCl_HH
#define TrackCl_HH

#include <Rtypes.h>
#include <TObject.h>

class TrackCl : public TObject {
public:
  // Global Information
  int run,evn;
  float zdcz,zdct0,zdcch;
  float bbcz,bbct0,bbcch,bbcnhit;
  float pc1nhit;
  float emcnhit;
  
  // Tracking Information
  float inters,ptot,cglarm,quality;
  float proj[3],dir[3],pathl;
  float alpha,beta;

  // EMCal Clustering information
  float arm,sector,twrhit;
  float clustno,nsh,tof,e,ecent,ecore,ecorr,tofcorr,chi2;
  float pos[3],disp[2],padisp[2],ind[2];
  // Association information
  float drmin,newproj[3];
  float corpos[3],corpathl;

  // EMCal Clustering informatino after Z-flip
  float arm_s,sector_s,twrhit_s;
  float clustno_s,nsh_s,tof_s,e_s,ecent_s,ecore_s,ecorr_s,tofcorr_s,chi2_s;
  float pos_s[3],disp_s[2],padisp_s[2],ind_s[2];
  // Association information
  float drmin_s,newproj_s[3];
  float corpos_s[3],corpathl_s;

  // RICH information
  int crk_acc,crk_npmt0,crk_npmt1;
  float crk_npe0,crk_npe1,crk_chi2,crk_disp;

  // RICH information after Z-flip
  int crk_acc_s,crk_npmt0_s,crk_npmt1_s;
  float crk_npe0_s,crk_npe1_s,crk_chi2_s,crk_disp_s;

public:
  TrackCl();
  void Reset();

  ClassDef(TrackCl,1)
};
//
#endif
//
