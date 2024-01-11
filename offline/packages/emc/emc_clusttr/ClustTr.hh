#ifndef ClustTr_HH
#define ClustTr_HH

//#include <Rtypes.h>
#include <TObject.h>

class ClustTr : public TObject {
public:
  // Global Information
  int run,evn;
  float zdcz,zdct0,zdcch;
  float bbcz,bbct0,bbcch,bbcnhit;
  float pc1nhit;
  float emcnhit;
  
  // EMCal Clustering information
  int arm,sector,twrhit,clustno;
  float nsh,tof,e,ecent,ecore,ecorr,tofcorr,chi2;
  float pos[3],disp[2],padisp[2],ind[2];

  // Tracking Information
  float inters,ptot,cglarm,quality;
  float proj[3],dir[3];
  // RICH information
  int crk_acc,crk_npmt0,crk_npmt1;
  float crk_npe0,crk_npe1,crk_chi2,crk_disp;
  // Association information
  float drmin,newproj[3],corpos[3];
  float pathl;

  // Tracking Information after Z-flip
  float inters_s,ptot_s,cglarm_s,quality_s;
  float proj_s[3],dir_s[3];
  // RICH information after Z-flip
  int crk_acc_s,crk_npmt0_s,crk_npmt1_s;
  float crk_npe0_s,crk_npe1_s,crk_chi2_s,crk_disp_s;
  // Association information
  float pos_s[3];
  float drmin_s,newproj_s[3],corpos_s[3];
  float pathl_s;

public:
  ClustTr();
  void Reset();

  ClassDef(ClustTr,1)
};
//
#endif
//
