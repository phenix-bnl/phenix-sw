////////////////////////////////////////////////////////////////
//  DchMcRecoTrack.hh
//  Author:Federica Messer 
//  $Log: DchMcRecoTrack.hh,v $
//  Revision 1.12  2002/05/12 03:04:26  jjia
//  evalator was modified
//
//  Revision 1.11  2002/05/02 23:48:11  jjia
//  add new variables
//
//  Revision 1.10  2002/04/03 14:35:54  dave
//  Clean ups.  Extra blank lines, commented-out code, removed.
//
//  Revision 1.9  2002/03/18 23:40:33  dave
//  Cleaned out commented-out dead code.  Really.  No functional lines of
//  code were harmed in this checkin.  Not one.  It still compiles.
//
//  Revision 1.8  2002/02/23 03:59:29  momchil
//  merged the latest changes of prod_2_4 into the HEAD branch
//
//  Revision 1.5.2.2  2002/02/20 22:13:05  pinkenbu
//  Here comes the head branch...
//
//  Revision 1.7  2002/02/20 20:28:49  pinkenbu
//  Make destructors virtual (as per longstanding C++ guru advisory)
//
//  Revision 1.6  2002/02/13 04:11:10  jjia
//  add phivtx to evaluation ntuple
//
//  Revision 1.5  2001/12/09 13:40:42  jjia
//  extend the evaluation such that it contains Pc2 and also matching in term of number of sigmas for various subsystem
//
////////////////////////////////////////////////////////////////

#ifndef _DCHMCRECOTRACK_H
#define _DCHMCRECOTRACK_H

class DchMcRecoTrack {
  
private:

  int eventID;
  float eventxvtx;
  float eventyvtx;
  float eventzvtx;
  int perfID;
  int perfQual;
  float momentumG;
  float theta;
  float phi;
  float theta0G;
  float phi0G;
  float alphaG;
  float betaG;
  float zedG;
  int generation;
  int particleID;
  int parentID;
  int primaryID;
  float rvtx;
  float zvtx;
  float phivtx;
    
  int recoID;
  int recoQual;
  float momentumR;
  float theta0R;
  float phi0R;
  int xhits;
  int uvhits;
  int mulcontrib;
  int xmulcontrib;
  int uvmulcontrib;

  int mainID;
  int xmainID;
  int uvmainID;
  int ambig;
  int xambig;
  int uvambig;
  float purity;
  float xpurity;
  float uvpurity;
  float dalpha;

  float dbeta;
  float dphi;
  float dzed;
  float ddist;
  int sumfound;
  int solution;
  float perfDvertex;
  float recoDvertex;
  float chi2;
  int numHitsFit;
  float dalphaMin;
  float dphiMin;
  float avDist;
 
  int idGeantTrack;

  int pc1RecoId;
  int pc2RecoId;
  int pc3RecoId;
  int emcRecoId;
  int tecRecoId;
  int tofRecoId;
   
  // if correct association then  McId == RecoId
  int pc1McId; // the (Reconstructed) id that correspond to the MC hit
  int pc2McId;
  int pc3McId;
  int emcMcId;
  int tecMcId;
  int tofMcId;
 
  float xPc1Reco;
  float yPc1Reco;
  float zPc1Reco;

  float xPc2Reco;
  float yPc2Reco;
  float zPc2Reco;

  float xPc3Reco;
  float yPc3Reco;
  float zPc3Reco;

  float xTecReco;
  float yTecReco;
  float zTecReco;

  float xEmcReco;
  float yEmcReco;
  float zEmcReco;

  float xTofReco;
  float yTofReco;
  float zTofReco;

  //---------------------
  float xPc1Mc;
  float yPc1Mc;
  float zPc1Mc;

  float xPc2Mc;
  float yPc2Mc;
  float zPc2Mc;

  float xPc3Mc;
  float yPc3Mc;
  float zPc3Mc;

  float xTecMc;
  float yTecMc;
  float zTecMc;

  float xEmcMc;
  float yEmcMc;
  float zEmcMc;

  float xTofMc;
  float yTofMc;
  float zTofMc;

  float xPc1Proj;
  float yPc1Proj;
  float zPc1Proj;

  float zPc3Proj;
  float yPc3Proj;
  float xPc3Proj;

  float xPc2Proj;
  float yPc2Proj;
  float zPc2Proj;

  float xTofProj;
  float yTofProj;
  float zTofProj;
  
  float xEmcProj;
  float yEmcProj;
  float zEmcProj;

  float xTecProj;
  float yTecProj;
  float zTecProj;

  float pc13vtxr;
  float pc13vtxm;
  float bbcvtx;
  float bbct0;

  // TOF additionals
  float pathlTof;
  float tofTofMc, tofTofReco;
  float elossTofMc, elossTofReco;
  int pidTofMc, pidTofReco;
  // EMC additionals
  float pathlEmc;
  float emcmease, emcecore, emcecorr, emcecent, emctof, emctofcorr, emctofmin, emcprobphot;
  float emcchi2, emcpartesum0, emcpartesum1, emcpartesum2, emcpartesum3;
  float emcAncedep0, emcAncedep1, emcAncedep2;
  float emcAncptot0, emcAncptot1, emcAncptot2;
  float emcMcefrac, emcMcecore, emcMcmease, emcMctof;
  int emcswkey, twrhit;
  int emcAnctrkno0, emcAnctrkno1, emcAnctrkno2;
  int emcAnctwrhit0, emcAnctwrhit1, emcAnctwrhit2;
  int emcAncpid0, emcAncpid1, emcAncpid2;
  // CRK additionals
  int crkacc, crknpmt0, crknpmt1, crknpmt3;
  float crknpe0, crknpe1, crknpe3, crkchi2, crkdisp, crkpath;

public:
  //additional information about the track
  int hitid[40];

  //information for decays/or parent;
  int   _idG0,_pid0,_nx10,_idpare0,_gen0;
  float _p0,_pthe0,_pphi0;
  int   _idG1,_idG2; 
  float _p1,_p2,_pthe1,_pthe2,_pphi1,_pphi2;
  int   _Q1,_Q2;
  int   _pid1,_pid2; 
  float _phi1,_phi2; 
  float _zed1,_zed2;
  short _nx11,_nx21,_nx12,_nx22;
  short _nx1G1,_nx2G1,_nx1G2,_nx2G2;
  float _alp1G1,_alp2G1,_alp1G2,_alp2G2;
  float _xPc1Reco1,_yPc1Reco1,_zPc1Reco1,_xPc1Reco2,_yPc1Reco2,_zPc1Reco2;
  float _xPc2Reco1,_yPc2Reco1,_zPc2Reco1,_xPc2Reco2,_yPc2Reco2,_zPc2Reco2;
  float _xPc3Reco1,_yPc3Reco1,_zPc3Reco1,_xPc3Reco2,_yPc3Reco2,_zPc3Reco2;
  float _xPc1Mc1,_yPc1Mc1,_zPc1Mc1,_xPc1Mc2,_yPc1Mc2,_zPc1Mc2;
  float _xPc2Mc1,_yPc2Mc1,_zPc2Mc1,_xPc2Mc2,_yPc2Mc2,_zPc2Mc2;
  float _xPc3Mc1,_yPc3Mc1,_zPc3Mc1,_xPc3Mc2,_yPc3Mc2,_zPc3Mc2;

public:
  //for matching
  float ntrkG,ntrkR;
  float sigpc1,sigpc1p,sigpc1z,delpc1p,delpc1z;
  float sigpc2,sigpc2p,sigpc2z,delpc2p,delpc2z;
  float sigpc3,sigpc3p,sigpc3z,delpc3p,delpc3z;
  float sigtof,sigtofp,sigtofz,deltofp,deltofz;
  float sigemc,sigemcp,sigemcz,delemcp,delemcz;
  //additional track information
  float alpha1,alpha2,alpha1G,alpha2G;
  int   nx1,nx2,nx1G,nx2G;
  float mdist1,mdist2,mdist1G,mdist2G;
  float chi21,chi22,chi21G,chi22G;
public:
  
  DchMcRecoTrack(); 
  virtual ~DchMcRecoTrack();

  void fill(float* array);

  // --------------------- GET methods
  int   get_eventID() {return eventID;}
  float get_eventxvtx() {return eventxvtx;}
  float get_eventyvtx() {return eventyvtx;}
  float get_eventzvtx() {return eventzvtx;}
  int   get_perfID() {return perfID;}
  int   get_perfQual() {return perfQual;}
  float get_momentumG() {return momentumG;}
  float get_theta() {return theta;}
  float get_phi() {return phi;}
  float get_theta0G() {return theta0G;}
  float get_phi0G() {return phi0G;}
  float get_alphaG() {return alphaG;}
  float get_betaG() {return betaG;}
  float get_zedG() {return zedG;}
  int   get_generation() {return generation;}
  int   get_particleID() {return particleID;}
  int   get_parentID() {return parentID;}
  int   get_primaryID() {return primaryID;}
  float get_rvtx() {return rvtx;}
  float get_zvtx() {return zvtx;}
  float get_phivtx() {return phivtx;}
    
  int   get_recoID() {return recoID;}
  int   get_recoQual() {return recoQual;}
  float get_momentumR() {return momentumR;}
  float get_theta0R() {return theta0R;}
  float get_phi0R() {return phi0R;}
  int   get_xhits() {return xhits;}
  int   get_uvhits() {return uvhits;}
  int   get_mulcontrib() {return mulcontrib;}
  int   get_xmulcontrib() {return xmulcontrib;}
  int   get_uvmulcontrib() {return uvmulcontrib;}

  int   get_mainID() {return mainID;}
  int   get_xmainID() {return xmainID;}
  int   get_uvmainID() {return uvmainID;}
  int   get_ambig() {return ambig;}
  int   get_xambig() {return xambig;}
  int   get_uvambig() {return uvambig;}
  float get_purity() {return purity;}
  float get_xpurity() {return xpurity;}
  float get_uvpurity() {return uvpurity;}
  float get_dalpha() {return dalpha;}

  float get_dbeta() {return dbeta;}
  float get_dphi() {return dphi;}
  float get_dzed() {return dzed;}
  float get_ddist() {return ddist;}
  int   get_sumfound() {return sumfound;}
  int   get_solution() {return solution;}
  float get_perfDvertex() {return perfDvertex;}
  float get_recoDvertex() {return recoDvertex;}
  float get_chi2() {return chi2;}
  int   get_numHitsFit() {return numHitsFit;}
  float get_dalphaMin() {return dalphaMin;}
  float get_dphiMin() {return dphiMin;}
  float get_avDist() {return avDist;}
 
  //-------------------------------SET methods
  
  void set_idGeantTrack(int val) {idGeantTrack = val;}
  int  get_idGeantTrack() {return idGeantTrack;}

  void set_eventID(int  val)   {eventID= val;}
  void set_eventxvtx(float val) {eventxvtx= val;}
  void set_eventyvtx(float val) {eventyvtx= val;}
  void set_eventzvtx(float val) {eventzvtx= val;}
  void set_perfID(int  val) {perfID= val;}
  void set_perfQual(int  val) {perfQual= val;}
  void set_momentumG(float val) {momentumG= val;}
  void set_theta(float val) {theta= val;}
  void set_phi(float val) {phi= val;}
  void set_theta0G(float val) {theta0G= val;}
  void set_phi0G(float val) {phi0G= val;}
  void set_alphaG( float val) {alphaG= val;}
  void set_betaG( float val) {betaG= val;}
  void set_zedG( float val) {zedG= val;}
  void set_generation(int  val) {generation= val;}
  void set_particleID(int  val) {particleID= val;}
  void set_parentID(int  val) {parentID= val;}
  void set_primaryID(int  val) {primaryID= val;}
  void set_rvtx(float val) {rvtx= val;}
  void set_zvtx(float val) {zvtx= val;}
  void set_phivtx(float val) {phivtx= val;}
  
  void set_recoID(int  val) {recoID= val;}
  void set_recoQual(int  val) {recoQual= val;}
  void set_momentumR( float val) {momentumR= val;}
  void set_theta0R(float val) {theta0R= val;}
  void set_phi0R( float val) {phi0R= val;}
  void set_xhits(int  val) {xhits= val;}
  void set_uvhits(int  val) {uvhits= val;}
  void set_mulcontrib(int  val) {mulcontrib= val;}
  void set_xmulcontrib(int  val) {xmulcontrib= val;}
  void set_uvmulcontrib(int  val) {uvmulcontrib= val;}
  
  void set_mainID(int  val) {mainID= val;}
  void set_xmainID(int  val) {xmainID= val;}
  void set_uvmainID(int  val) {uvmainID= val;}
  void set_ambig(int val) {ambig= val;}
  void set_xambig(int val) {xambig= val;}
  void set_uvambig(int val) {uvambig= val;}
  void set_purity(float val) {purity= val;}
  void set_xpurity( float val) {xpurity= val;}
  void set_uvpurity( float val) {uvpurity= val;}
  void set_dalpha( float val) {dalpha= val;}
  
  void set_dbeta(float val) {dbeta= val;}
  void set_dphi(float val) {dphi= val;}
  void set_dzed( float val) {dzed= val;}
  void set_ddist( float val) {ddist= val;}
  void set_sumfound(int  val) {sumfound= val;}
  void set_solution(int  val) {solution= val;}
  void set_perfDvertex(float val) {perfDvertex= val;}
  void set_recoDvertex(float val) {recoDvertex= val;}
  void set_chi2(float val) {chi2= val;}
  void set_numHitsFit(int  val) {numHitsFit= val;}
  void set_dalphaMin(float val) {dalphaMin = val;}
  void set_dphiMin(float val) {dphiMin = val;}
  void set_avDist(float val) {avDist = val;}

  //---------------------------------------
  // Introduced later for global checks
  //-------------------------------------

  int get_pc1RecoId() {return pc1RecoId;}
  int get_pc2RecoId() {return pc2RecoId;}
  int get_pc3RecoId() {return pc3RecoId;}
  int get_emcRecoId() {return emcRecoId;}
  int get_tecRecoId() {return tecRecoId;}
  int get_tofRecoId() {return tofRecoId;}
   
  int get_pc1McId() {return pc1McId;} 
  int get_pc2McId() {return pc2McId;}
  int get_pc3McId() {return pc3McId;}
  int get_emcMcId() {return emcMcId;}
  int get_tecMcId() {return tecMcId;}
  int get_tofMcId() {return tofMcId;}
 
  float get_xPc1Reco() {return xPc1Reco;}
  float get_yPc1Reco() {return yPc1Reco;}
  float get_zPc1Reco() {return zPc1Reco;}
  float get_xPc2Reco() {return xPc2Reco;}
  float get_yPc2Reco() {return yPc2Reco;}
  float get_zPc2Reco() {return zPc2Reco;}
  float get_xPc3Reco() {return xPc3Reco;}
  float get_yPc3Reco() {return yPc3Reco;}
  float get_zPc3Reco() {return zPc3Reco;}
  float get_xTecReco() {return xTecReco;}
  float get_yTecReco() {return yTecReco;}
  float get_zTecReco() {return zTecReco;}
  float get_xEmcReco() {return xEmcReco;}
  float get_yEmcReco() {return yEmcReco;}
  float get_zEmcReco() {return zEmcReco;}
  float get_xTofReco() {return xTofReco;}
  float get_yTofReco() {return yTofReco;}
  float get_zTofReco() {return zTofReco;}

  //---------------------
  float get_xPc1Mc() {return xPc1Mc;}
  float get_yPc1Mc() {return yPc1Mc;}
  float get_zPc1Mc() {return zPc1Mc;}
  float get_xPc2Mc() {return xPc2Mc;}
  float get_yPc2Mc() {return yPc2Mc;}
  float get_zPc2Mc() {return zPc2Mc;}
  float get_xPc3Mc() {return xPc3Mc;}
  float get_yPc3Mc() {return yPc3Mc;}
  float get_zPc3Mc() {return zPc3Mc;}
  float get_xTecMc() {return xTecMc;}
  float get_yTecMc() {return yTecMc;}
  float get_zTecMc() {return zTecMc;}
  float get_xEmcMc() {return xEmcMc;}
  float get_yEmcMc() {return yEmcMc;}
  float get_zEmcMc() {return zEmcMc;}
  float get_xTofMc() {return xTofMc;}
  float get_yTofMc() {return yTofMc;}
  float get_zTofMc() {return zTofMc;}
  //-----------------------------------------------------

  void set_pc1RecoId(int val) {pc1RecoId=val;}
  void set_pc2RecoId(int val) {pc2RecoId=val;}
  void set_pc3RecoId(int val) {pc3RecoId=val;}
  void set_emcRecoId(int val) {emcRecoId=val;}
  void set_tecRecoId(int val) {tecRecoId=val;}
  void set_tofRecoId(int val) {tofRecoId=val;}
  
  void set_pc1McId(int val) {pc1McId=val;} 
  void set_pc2McId(int val) {pc2McId=val;}
  void set_pc3McId(int val) {pc3McId=val;}
  void set_emcMcId(int val) {emcMcId=val;}
  void set_tecMcId(int val) {tecMcId=val;}
  void set_tofMcId(int val) {tofMcId=val;}
 
  void set_xPc1Reco(float val) {xPc1Reco=val;}
  void set_yPc1Reco(float val) {yPc1Reco=val;}
  void set_zPc1Reco(float val) {zPc1Reco=val;}
  void set_xPc2Reco(float val) {xPc2Reco=val;}
  void set_yPc2Reco(float val) {yPc2Reco=val;}
  void set_zPc2Reco(float val) {zPc2Reco=val;}
  void set_xPc3Reco(float val) {xPc3Reco=val;}
  void set_yPc3Reco(float val) {yPc3Reco=val;}
  void set_zPc3Reco(float val) {zPc3Reco=val;}
  void set_xTecReco(float val) {xTecReco=val;}
  void set_yTecReco(float val) {yTecReco=val;}
  void set_zTecReco(float val) {zTecReco=val;}
  void set_xEmcReco(float val) {xEmcReco=val;}
  void set_yEmcReco(float val) {yEmcReco=val;}
  void set_zEmcReco(float val) {zEmcReco=val;}
  void set_xTofReco(float val) {xTofReco=val;}
  void set_yTofReco(float val) {yTofReco=val;}
  void set_zTofReco(float val) {zTofReco=val;}
  
  //----------------
  void set_xPc1Mc(float val) {xPc1Mc=val;}
  void set_yPc1Mc(float val) {yPc1Mc=val;}
  void set_zPc1Mc(float val) {zPc1Mc=val;}
  void set_xPc2Mc(float val) {xPc2Mc=val;}
  void set_yPc2Mc(float val) {yPc2Mc=val;}
  void set_zPc2Mc(float val) {zPc2Mc=val;}
  void set_xPc3Mc(float val) {xPc3Mc=val;}
  void set_yPc3Mc(float val) {yPc3Mc=val;}
  void set_zPc3Mc(float val) {zPc3Mc=val;}
  void set_xTecMc(float val) {xTecMc=val;}
  void set_yTecMc(float val) {yTecMc=val;}
  void set_zTecMc(float val) {zTecMc=val;}
  void set_xEmcMc(float val) {xEmcMc=val;}
  void set_yEmcMc(float val) {yEmcMc=val;}
  void set_zEmcMc(float val) {zEmcMc=val;}
  void set_xTofMc(float val) {xTofMc=val;}
  void set_yTofMc(float val) {yTofMc=val;}
  void set_zTofMc(float val) {zTofMc=val;}

  //---------------------------------
  float get_xPc1Proj() {return xPc1Proj;}
  float get_yPc1Proj() {return yPc1Proj;}
  float get_zPc1Proj() {return zPc1Proj;}

  float get_xPc3Proj() {return xPc3Proj;}
  float get_yPc3Proj() {return yPc3Proj;}
  float get_zPc3Proj() {return zPc3Proj;}

  float get_xPc2Proj() {return xPc2Proj;}
  float get_yPc2Proj() {return yPc2Proj;}
  float get_zPc2Proj() {return zPc2Proj;}
  
  float get_xTofProj() {return xTofProj;}
  float get_yTofProj() {return yTofProj;}
  float get_zTofProj() {return zTofProj;}

  float get_zEmcProj() {return zEmcProj;}
  float get_yEmcProj() {return yEmcProj;}
  float get_xEmcProj() {return xEmcProj;}

  float get_xTecProj() {return xTecProj;}
  float get_yTecProj() {return yTecProj;}
  float get_zTecProj() {return zTecProj;}

  void set_xPc1Proj(float val) {xPc1Proj = val;}
  void set_yPc1Proj(float val) {yPc1Proj = val;}
  void set_zPc1Proj(float val) {zPc1Proj = val;}

  void set_xPc3Proj(float val) {xPc3Proj = val;}
  void set_yPc3Proj(float val) {yPc3Proj = val;}
  void set_zPc3Proj(float val) {zPc3Proj = val;}

  void set_xPc2Proj(float val) {xPc2Proj = val;}
  void set_yPc2Proj(float val) {yPc2Proj = val;}
  void set_zPc2Proj(float val) {zPc2Proj = val;}

  void set_xTofProj(float val) {xTofProj = val;}
  void set_yTofProj(float val) {yTofProj = val;}
  void set_zTofProj(float val) {zTofProj = val;}

  void set_xTecProj(float val) {xTecProj = val;}
  void set_yTecProj(float val) {yTecProj = val;}
  void set_zTecProj(float val) {zTecProj = val;}

  void set_xEmcProj(float val) {xEmcProj = val;}
  void set_yEmcProj(float val) {yEmcProj = val;}
  void set_zEmcProj(float val) {zEmcProj = val;}

  //

  float get_bbcvtx() {return bbcvtx;}
  void  set_bbcvtx(float val) {bbcvtx = val;}
  float get_bbct0() {return bbct0;}
  void  set_bbct0(float val) {bbct0 = val;}
  float get_pc13vtxr() {return pc13vtxr;}
  float get_pc13vtxm() {return pc13vtxm;}
  void set_pc13vtxr(float val) {pc13vtxr = val;}
  void set_pc13vtxm(float val) {pc13vtxm = val;}

  void set_tofTofMc(float val) {tofTofMc = val;}
  void set_elossTofMc(float val) {elossTofMc = val;}
  void set_pidTofMc(int val) {pidTofMc =val;}

  float get_tofTofMc() {return tofTofMc;}
  float get_elossTofMc() {return elossTofMc;}
  int get_pidTofMc() {return pidTofMc;}

  void set_pathlTof(float val) {pathlTof = val;}
  void set_tofTofReco(float val) {tofTofReco = val;}
  void set_elossTofReco(float val) {elossTofReco = val;}
  void set_pidTofReco(int val) {pidTofReco =val;}

  float get_pathlTof() {return pathlTof;}
  float get_tofTofReco() {return tofTofReco;}
  float get_elossTofReco() {return elossTofReco;}
  int get_pidTofReco() {return pidTofReco;}

  void set_pathlEmc(float val) {pathlEmc = val;}
  void set_emcmease(float val) {emcmease = val;}
  void set_emcecore(float val) {emcecore = val;}
  void set_emcecorr(float val) {emcecorr = val;}
  void set_emcecent(float val) {emcecent = val;}
  void set_emctof(float val) {emctof = val;}
  void set_emctofcorr(float val) {emctofcorr = val;}
  void set_emctofmin(float val) {emctofmin = val;}
  void set_emcprobphot(float val) {emcprobphot = val;}
  void set_emcchi2(float val) {emcchi2 = val;}
  void set_emcpartesum0(float val) {emcpartesum0 = val;}
  void set_emcpartesum1(float val) {emcpartesum1 = val;}
  void set_emcpartesum2(float val) {emcpartesum2 = val;}
  void set_emcpartesum3(float val) {emcpartesum3 = val;}
  void set_emcAncedep0(float val) {emcAncedep0 = val;}
  void set_emcAncedep1(float val) {emcAncedep1 = val;}
  void set_emcAncedep2(float val) {emcAncedep2 = val;}
  void set_emcAncptot0(float val) {emcAncptot0 = val;}
  void set_emcAncptot1(float val) {emcAncptot1 = val;}
  void set_emcAncptot2(float val) {emcAncptot2 = val;}
  void set_emcMcefrac(float val) {emcMcefrac = val;}
  void set_emcMcecore(float val) {emcMcecore = val;}
  void set_emcMcmease(float val) {emcMcmease = val;}
  void set_emcMctof(float val) {emcMctof = val;}
  void set_emcswkey(int val) {emcswkey = val;}
  void set_twrhit(int val) {twrhit = val;}
  void set_emcAnctrkno0(int val) {emcAnctrkno0 = val;}
  void set_emcAnctrkno1(int val) {emcAnctrkno1 = val;}
  void set_emcAnctrkno2(int val) {emcAnctrkno2 = val;}
  void set_emcAnctwrhit0(int val) {emcAnctwrhit0 = val;}
  void set_emcAnctwrhit1(int val) {emcAnctwrhit1 = val;}
  void set_emcAnctwrhit2(int val) {emcAnctwrhit2 = val;}
  void set_emcAncpid0(int val) {emcAncpid0 = val;}
  void set_emcAncpid1(int val) {emcAncpid1 = val;}
  void set_emcAncpid2(int val) {emcAncpid2 = val;}

  float get_pathlEmc() {return pathlEmc;}
  float get_emcmease() {return emcmease;}
  float get_emcecore() {return emcecore;}
  float get_emcecorr() {return emcecorr;}
  float get_emcecent() {return emcecent;}
  float get_emctof() {return emctof;}
  float get_emctofcorr() {return emctofcorr;}
  float get_emctofmin() {return emctofmin;}
  float get_emcprobphot() {return emcprobphot;}
  float get_emcchi2() {return emcchi2;}
  float get_emcpartesum0() {return emcpartesum0;}
  float get_emcpartesum1() {return emcpartesum1;}
  float get_emcpartesum2() {return emcpartesum2;}
  float get_emcpartesum3() {return emcpartesum3;}
  float get_emcAncedep0() {return emcAncedep0;}
  float get_emcAncedep1() {return emcAncedep1;}
  float get_emcAncedep2() {return emcAncedep2;}
  float get_emcAncptot0() {return emcAncptot0;}
  float get_emcAncptot1() {return emcAncptot1;}
  float get_emcAncptot2() {return emcAncptot2;}
  float get_emcMcefrac() {return emcMcefrac;}
  float get_emcMcecore() {return emcMcecore;}
  float get_emcMcmease() {return emcMcmease;}
  float get_emcMctof() {return emcMctof;}
  int get_emcswkey() {return emcswkey;}
  int get_twrhit() {return twrhit;}
  int get_emcAnctrkno0() {return emcAnctrkno0;}
  int get_emcAnctrkno1() {return emcAnctrkno1;}
  int get_emcAnctrkno2() {return emcAnctrkno2;}
  int get_emcAnctwrhit0() {return emcAnctwrhit0;}
  int get_emcAnctwrhit1() {return emcAnctwrhit1;}
  int get_emcAnctwrhit2() {return emcAnctwrhit2;}
  int get_emcAncpid0() {return emcAncpid0;}
  int get_emcAncpid1() {return emcAncpid1;}
  int get_emcAncpid2() {return emcAncpid2;}

  void set_crkacc(int val) {crkacc = val;}
  void set_crknpmt0(int val) {crknpmt0 = val;}
  void set_crknpmt1(int val) {crknpmt1 = val;}
  void set_crknpmt3(int val) {crknpmt3 = val;}
  void set_crknpe0(float val) {crknpe0 = val;}
  void set_crknpe1(float val) {crknpe1 = val;}
  void set_crknpe3(float val) {crknpe3 = val;}
  void set_crkchi2(float val) {crkchi2 = val;}
  void set_crkdisp(float val) {crkdisp = val;}
  void set_crkpath(float val) {crkpath = val;}

  int get_crkacc() {return crkacc;}
  int get_crknpmt0() {return crknpmt0;}
  int get_crknpmt1() {return crknpmt1;}
  int get_crknpmt3() {return crknpmt3;}
  float get_crknpe0() {return crknpe0;}
  float get_crknpe1() {return crknpe1;}
  float get_crknpe3() {return crknpe3;}
  float get_crkchi2() {return crkchi2;}
  float get_crkdisp() {return crkdisp;}
  float get_crkpath() {return crkpath;}

};
#endif /* _DCHMCRECOTRACK_H */














