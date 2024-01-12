#ifndef __EMCPIDRECAL_DAU_RECO_H__
#define __EMCPIDRECAL_DAU_RECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class TH2;

class EmcPidrecal_dAu_Reco : public Recalibrator
{
  // This is a recalibrator for dAu 200GeV from Run3
  // It is based off of the code written by D. Kochetkov
  // in offline/analysis/EMC_PbSc_tof_run3dAu which is the
  // public code that provides the EMC tof corrections for 
  // run3 dAu analyses according to an466
  //  --SCC 1/23/2006

 public:
  EmcPidrecal_dAu_Reco(const std::string &name="EmcPidrecal_dAu_Reco");
  virtual ~EmcPidrecal_dAu_Reco() {}

  //  Standard methods
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  
 protected:
  float Correct_temc(float temc, const float mom, const float ecent, const float alpha, 
		     const int dcarm, const int sect, const int ysect, const int zsect);

  float IsPion  (const float m2emc, const float mom, const float alpha, const int dcarm, const int sect);
  float IsKaon  (const float m2emc, const float mom, const float alpha, const int dcarm, const int sect);
  float IsProton(const float m2emc, const float mom, const float alpha, const int dcarm, const int sect);

  void SetCalibConsts();

  TH2 *dAuEMCmomtof;
  TH2 *dAuEMCmomtofP;
  TH2 *dAuEMCmomtofK;
  TH2 *dAuEMCmomtofPi;

  //SET IN InitRun
  // per run values
  int runnumber;
  float Mw0p,Mw1p,Mw2p,Mw3p,Me2p,Me3p;
  float Mw0n,Mw1n,Mw2n,Mw3n,Me2n,Me3n;

  // SET IN SetCalibConsts
  // large array values -- for each of the 271 runs and then each sector and sign
//  static float RUN[271];
//  static float w0mp[271],w1mp[271],w2mp[271],w3mp[271],e2mp[271],e3mp[271];
//  static float w0mn[271],w1mn[271],w2mn[271],w3mn[271],e2mn[271],e3mn[271];
//
//  // hot or dead tower flags -- one flag for each ysect, zsect for each sector
//  int flagw0[72][36],flagw1[72][36],flagw2[72][36];
//  int flagw3[72][36],flage2[72][36],flage3[72][36];
//
//  // tower corrections -- one value for each ysect, zsect for each sector and sign
//  static float tw0p[72][36],tw1p[72][36],tw2p[72][36];
//  static float tw3p[72][36],te2p[72][36],te3p[72][36];
//  static float tw0n[72][36],tw1n[72][36],tw2n[72][36];
//  static float tw3n[72][36],te2n[72][36],te3n[72][36];
//
//  // slew correction parameters
//  static float w0p0,w0p1,w0p2;
//  static float w1p0,w1p1,w1p2;
//  static float w2p0,w2p1,w2p2;
//  static float w3p0,w3p1,w3p2;
//  static float w0n0,w0n1,w0n2;
//  static float w1n0,w1n1,w1n2;
//  static float w2n0,w2n1,w2n2;
//  static float w3n0,w3n1,w3n2;
//  static float e2p0,e2p1,e2p2;
//  static float e3p0,e3p1,e3p2;
//  static float e2n0,e2n1,e2n2;
//  static float e3n0,e3n1,e3n2;
//
//  // mom corrections for temc parameters
//  static float w0p0m,w0p1m,w0p2m;
//  static float w1p0m,w1p1m,w1p2m;
//  static float w2p0m,w2p1m,w2p2m;
//  static float w3p0m,w3p1m,w3p2m;
//  static float w0n0m,w0n1m,w0n2m;
//  static float w1n0m,w1n1m,w1n2m;
//  static float w2n0m,w2n1m,w2n2m;
//  static float w3n0m,w3n1m,w3n2m;
//  static float e2p0m,e2p1m,e2p2m;
//  static float e3p0m,e3p1m,e3p2m;
//  static float e2n0m,e2n1m,e2n2m;
//  static float e3n0m,e3n1m,e3n2m;
//  
//  // M2 fit parameters
//  static float w0mp0_pi,w1mp0_pi,w2mp0_pi,w3mp0_pi,e2mp0_pi,e3mp0_pi;
//  static float w0mp1_pi,w1mp1_pi,w2mp1_pi,w3mp1_pi,e2mp1_pi,e3mp1_pi;
//  static float w0mp2_pi,w1mp2_pi,w2mp2_pi,w3mp2_pi,e2mp2_pi,e3mp2_pi;
//  static float w0mp3_pi,w1mp3_pi,w2mp3_pi,w3mp3_pi,e2mp3_pi,e3mp3_pi;
//  static float w0mn0_pi,w1mn0_pi,w2mn0_pi,w3mn0_pi,e2mn0_pi,e3mn0_pi;
//  static float w0mn1_pi,w1mn1_pi,w2mn1_pi,w3mn1_pi,e2mn1_pi,e3mn1_pi;
//  static float w0mn2_pi,w1mn2_pi,w2mn2_pi,w3mn2_pi,e2mn2_pi,e3mn2_pi;
//  static float w0mn3_pi,w1mn3_pi,w2mn3_pi,w3mn3_pi,e2mn3_pi,e3mn3_pi;
//  static float w0sp0_pi,w1sp0_pi,w2sp0_pi,w3sp0_pi,e2sp0_pi,e3sp0_pi;
//  static float w0sp1_pi,w1sp1_pi,w2sp1_pi,w3sp1_pi,e2sp1_pi,e3sp1_pi;
//  static float w0sp2_pi,w1sp2_pi,w2sp2_pi,w3sp2_pi,e2sp2_pi,e3sp2_pi;
//  static float w0sn0_pi,w1sn0_pi,w2sn0_pi,w3sn0_pi,e2sn0_pi,e3sn0_pi;
//  static float w0sn1_pi,w1sn1_pi,w2sn1_pi,w3sn1_pi,e2sn1_pi,e3sn1_pi;
//  static float w0sn2_pi,w1sn2_pi,w2sn2_pi,w3sn2_pi,e2sn2_pi,e3sn2_pi;
//  static float w0mp0_K,w1mp0_K,w2mp0_K,w3mp0_K,e2mp0_K,e3mp0_K;
//  static float w0mp1_K,w1mp1_K,w2mp1_K,w3mp1_K,e2mp1_K,e3mp1_K;
//  static float w0mp2_K,w1mp2_K,w2mp2_K,w3mp2_K,e2mp2_K,e3mp2_K;
//  static float w0mn0_K,w1mn0_K,w2mn0_K,w3mn0_K,e2mn0_K,e3mn0_K;
//  static float w0mn1_K,w1mn1_K,w2mn1_K,w3mn1_K,e2mn1_K,e3mn1_K;
//  static float w0mn2_K,w1mn2_K,w2mn2_K,w3mn2_K,e2mn2_K,e3mn2_K;
//  static float w0sp0_K,w1sp0_K,w2sp0_K,w3sp0_K,e2sp0_K,e3sp0_K;
//  static float w0sp1_K,w1sp1_K,w2sp1_K,w3sp1_K,e2sp1_K,e3sp1_K;
//  static float w0sp2_K,w1sp2_K,w2sp2_K,w3sp2_K,e2sp2_K,e3sp2_K;
//  static float w0sn0_K,w1sn0_K,w2sn0_K,w3sn0_K,e2sn0_K,e3sn0_K;
//  static float w0sn1_K,w1sn1_K,w2sn1_K,w3sn1_K,e2sn1_K,e3sn1_K;
//  static float w0sn2_K,w1sn2_K,w2sn2_K,w3sn2_K,e2sn2_K,e3sn2_K;
//  static float w0mp0_p,w1mp0_p,w2mp0_p,w3mp0_p,e2mp0_p,e3mp0_p;
//  static float w0mp1_p,w1mp1_p,w2mp1_p,w3mp1_p,e2mp1_p,e3mp1_p;
//  static float w0mp2_p,w1mp2_p,w2mp2_p,w3mp2_p,e2mp2_p,e3mp2_p;
//  static float w0mn0_p,w1mn0_p,w2mn0_p,w3mn0_p,e2mn0_p,e3mn0_p;
//  static float w0mn1_p,w1mn1_p,w2mn1_p,w3mn1_p,e2mn1_p,e3mn1_p;
//  static float w0mn2_p,w1mn2_p,w2mn2_p,w3mn2_p,e2mn2_p,e3mn2_p;
//  static float w0sp0_p,w1sp0_p,w2sp0_p,w3sp0_p,e2sp0_p,e3sp0_p;
//  static float w0sp1_p,w1sp1_p,w2sp1_p,w3sp1_p,e2sp1_p,e3sp1_p;
//  static float w0sp2_p,w1sp2_p,w2sp2_p,w3sp2_p,e2sp2_p,e3sp2_p;
//  static float w0sn0_p,w1sn0_p,w2sn0_p,w3sn0_p,e2sn0_p,e3sn0_p;
//  static float w0sn1_p,w1sn1_p,w2sn1_p,w3sn1_p,e2sn1_p,e3sn1_p;
//  static float w0sn2_p,w1sn2_p,w2sn2_p,w3sn2_p,e2sn2_p,e3sn2_p;
//
  //  static int s;
  //  static float value[13],a;
  // static float at,bt,ct,dt;

  //static int aw0,bw0,cw0,aw1,bw1,cw1,aw2,bw2,cw2;
  // static int aw3,bw3,cw3,ae2,be2,ce2,ae3,be3,ce3;


  // static float b1,value2[84],b1_1,value2_1[72],b1_2,value2_2[72];
  //  static float values[36],as,valuem[36],am;
  
  
  
  
};

#endif /* __PIDRECAL_DAU_RECO_H__ */
