#ifndef __MPCCLUSTERCONTENTV2_H__
#define __MPCCLUSTERCONTENTV2_H__

#include <mpcClusterContent.h>
#include <string>
/** (VERSION) mpcClusterContent version 2. 

*/

class mpcClusterContentV2 : public mpcClusterContent
{
public:

  /// CREATORS.
  mpcClusterContentV2();
  mpcClusterContentV2(const mpcClusterContentV2&);
  mpcClusterContentV2& operator=(const mpcClusterContentV2&);

  mpcClusterContentV2* clone(void) const;
  mpcClusterContentV2* create(void) const;

  virtual ~mpcClusterContentV2();
  virtual void Clear(Option_t* option="");
  virtual void Clear_Sim(Option_t* option="");

  /// ACCESSORS.

  int arm() const { return fArm; }

  float chi2() const { return fChi2; }
  float chi2core() const { return fChi2core; }
  int ndfcore() const { return fNdfcore; }

  float corrdispy() const { return fCorrDispy; }
  // the below is now deprecated
  float corrdispz() const { return fCorrDispx; }
  float corrdispx() const { return fCorrDispx; }

  //v2 addition
  float corrlogdispy() const { return fCorrLogDispy; }
  // the below is now deprecated
  float corrlogdispz() const { return fCorrLogDispx; }
  float corrlogdispx() const { return fCorrLogDispx; }

  unsigned int cutword() const { return fCutword; }

  unsigned int deadmap() const { return fDeadmap; }
  
  float dispy() const { return fDispy; }
  // the below is now deprecated
  float dispz() const { return fDispx; }
  float dispx() const { return fDispx; }

  //v2 addition
  float logdispy() const { return fLogDispy; }
  float logdispz() const { return fLogDispx; }
  float logdispx() const { return fLogDispx; }

  float dx() const { return fDx; }  
  float dy() const { return fDy; }
  float dz() const { return fDz; }
  
  float e() const { return fE; }
  float e9() const { return fE9; }
  float ecore() const { return fEcore; }
  float ecent() const { return fEcent; }
  float etofmin() const { return fEtofmin; }
  float etofmax() const { return fEtofmax; }
  
  int id() const { return fId; }
  
  void identify(std::ostream& os=std::cout) const;
  
  int isValid() const;

  int ixpos() const { return fIxpos; }
  int iypos() const { return fIypos; }

  //v2 addition
  int ixhitpos() const { return fIxpos_hit; }
  int iyhitpos() const { return fIypos_hit; }
  
  float quality() const { return fQuality; }
  
  int multiplicity() const { return fMultiplicity; }
  
  float padispy() const { return fPadispy; }
  // the blow is deprecated
  float padispz() const { return fPadispx; }
  float padispx() const { return fPadispx; }
  
  // index ranges from 0 to multiplicity().
  float partesum(int index) const;
  
  float prob_photon() const { return fProb_photon; }
  
  float phi() const { return fPhi; }
  
  int pid() const { return fPid; }
  
  void print(std::ostream& out=std::cout) const;
  void print_pythia() const;

  float rawtdc() const { return fRawTDC; }

//  int sector() const { return fSector; }
  
  float simfrac() const { return 0; }

  float tof() const { return fTof; }
  float tofhad() const { return fTofHad; }
  float tofdisp() const { return fTofdisp; }
  float tofmin() const { return fTofmin; }
  float tofmax() const { return fTofmax; }
 
  float tofcorr() const { return fTofcorr; }
  float tofcorrmin() const { return fTofcorrmin; }
  float tofcorrmax() const { return fTofcorrmax; }
  
  float theta() const { return fTheta; }
  
  int towerid(int index) const;
  
//  int type() const { return fType; }
  
  unsigned int warnmap() const { return fWarnmap; }
  
  float x() const { return fX; }  
  float y() const { return fY; }
  float z() const { return fZ; }

  
  float linycg() const { return fLinYcg; }
  float linxcg() const { return fLinXcg; }
  float liny() const { return fLinY; }
  float linx() const { return fLinX; }

  float logycg() const { return fYcg; }
  float logxcg() const { return fXcg; }
  float logy() const { return fY; }
  float logx() const { return fX; }

  float ycgmin() const { return fYcgmin; }
  float xcgmin() const { return fXcgmin; }
  float ymin() const { return fYmin; }
  float xmin() const { return fXmin; }




  float ycg() const { return fYcg; }
  float xcg() const { return fXcg; }
  float zcg() const { return fXcg; }

  bool has_yz_cg() const { return true; }

  bool split() const { return fIsSplit; }
  float chi2_split() const { return fChi2_split; }
  float y1() const { return fY1; }
  float x1() const { return fX1; }
  float y2() const { return fY2; }
  float x2() const { return fX2; }
  float e1() const { return fE1; }
  float e2() const { return fE2; }
  
/*
  short emcpc3       () const { return femcpc3        ;}
  short emcpc3neartrk() const { return femcpc3neartrk ;}
  float emcpc3dz     () const { return femcpc3dz      ;}
  float emcpc3dphi   () const { return femcpc3dphi    ;}
  short emctrk       () const { return femctrk        ;}
  float emctrkdz     () const { return femctrkdz      ;}
  float emctrkdphi   () const { return femctrkdphi    ;}
  float pemctrk      () const { return fpemctrk       ;}
  short emctrkquality() const { return femctrkquality ;}
*/



  /// MUTATORS.
  
  void Reset();

  void set_arm(int arm) { fArm=arm; }
  void set_chi2(float chi2) { fChi2=chi2; }
  void set_chi2core(float chi2) { fChi2core=chi2; }
  void set_ndfcore(int ndf) {fNdfcore = ndf; }
  void set_corrdisp(float corrdispy, float corrdispx) { fCorrDispy=corrdispy; fCorrDispx=corrdispx; }
  //v2 addition
  void set_corrlogdisp(float corrdispy, float corrdispx) { fCorrLogDispy=corrdispy; fCorrLogDispx=corrdispx; }
  void set_cutword(unsigned int cw) { fCutword=cw; }
  void set_disp(float dispy, float dispx) { fDispy=dispy; fDispx=dispx; }
  //v2 addition
  void set_logdisp(float dispy, float dispx) { fLogDispy=dispy; fLogDispx=dispx; }
  void set_dxyz(float dx, float dy, float dz) { fDx=dx; fDy=dy; fDz=dz; }
  void set_e(float e) { fE=e;}
  void set_e9(float e9) { fE9=e9;}
  void set_ecore(float ecore) { fEcore=ecore; }
  void set_ecent(float ecent) { fEcent=ecent; }
  void set_etofmin(float etofmin) { fEtofmin=etofmin; }
  void set_etofmax(float etofmax) { fEtofmax=etofmax; }
  void set_id(int id) { fId=id; }
  void set_ipos(int ix, int iy) { fIxpos=ix; fIypos=iy; }
  //v2 addition
  void set_ihitpos(int ix, int iy) { fIxpos_hit=ix; fIypos_hit=iy; }
  void set_quality(float qual) { fQuality=qual; }
  void set_maps(unsigned int dead, unsigned int warn)
  { fDeadmap = dead; fWarnmap=warn; }
  void set_multiplicity(int mul);
  void set_padisp(float padispy, float padispx) 
  { fPadispy=padispy; fPadispx=padispx; }
  void set_partesum(int index, float value);
  void set_prob_photon(float prob) { fProb_photon = prob; }
  void set_phi(float phi) { fPhi=phi; }
  void set_pid(int pid) { fPid=pid; }
  void set_rawtdc(float rtdc) { fRawTDC=rtdc; }
  //void set_sector(int sector) { fSector=sector; }
  void set_tof(float tof) { fTof=tof; }  
  void set_tofhad(float tofhad) { fTofHad=tofhad; }
  void set_tofdisp(float tofdisp) { fTofdisp=tofdisp; }
  void set_tofmin(float tofmin) { fTofmin=tofmin; }
  void set_tofmax(float tofmax) { fTofmax=tofmax; }
  void set_tofcorr(float tofcorr) { fTofcorr=tofcorr; }
  void set_tofcorrmin(float tofcorrmin) { fTofcorrmin=tofcorrmin; }
  void set_tofcorrmax(float tofcorrmax) { fTofcorrmax=tofcorrmax; }
  void set_theta(float theta) { fTheta=theta;}
  void set_towerid(int index, int value);

  void set_xyz(float x, float y, float z) { fX=x; fY=y; fZ=z; }
  
  //deprecated
  void set_yz_cg(float ycg, float xcg) { fYcg=ycg; fXcg=xcg; }
  void set_yx_cg(float ycg, float xcg) { fYcg=ycg; fXcg=xcg; }

  void set_yx_logcg(float ycg, float xcg) { fYcg=ycg; fXcg=xcg;}
  void set_yx_log(float y, float x) { fY=y; fX=x;}

  void set_yx_lincg(float ycg, float xcg) { fLinYcg=ycg; fLinXcg=xcg; }
  void set_yx_lin(float y, float x) { fLinY=y; fLinX=x; }
  
  void set_yx_cgmin(float ycg, float xcg) { fYcgmin=ycg; fXcgmin=xcg;}
  void set_yx_min(float y, float x) { fYmin=y; fXmin=x; }


  void set_yx1(float y, float x) { fY1=y; fX1=x; }
  void set_yx2(float y, float x) { fY2=y; fX2=x; }
  void set_e12(float e1, float e2) { fE1=e1; fE2=e2; }
  void set_split(bool is_split) { fIsSplit = is_split;}
  void set_chi2_split(float chi2) { fChi2_split = chi2;}



  void set_sim_size(int size);
  void set_sim_type(short type) { fType=type; }
  void set_sim(int index, int it, int id, int pri, float edep, float frac,
	       int kf, int node, int parentkf, int parentid);

  
  int get_sim_size() const{ return fNsim;}
  short get_sim_type() const{ return fType; }
  void get_sim(int index, int& it, int& id, int& pri,float& edep,
	       float& frac, int& kf, int& node, 
	       int& parentkf, int& parentid) const;




  //v2 addition

/*
  void set_emcpc3       (short val) {femcpc3       = val;}
  void set_emcpc3neartrk(short val) {femcpc3neartrk= val;}
  void set_emcpc3dz     (float val) {femcpc3dz     = val;}
  void set_emcpc3dphi   (float val) {femcpc3dphi   = val;}
  void set_emctrk       (short val) {femctrk       = val;}
  void set_emctrkdz     (float val) {femctrkdz     = val;}
  void set_emctrkdphi   (float val) {femctrkdphi   = val;}
  void set_pemctrk      (float val) {fpemctrk      = val;}
  void set_emctrkquality(short val) {femctrkquality= val;}
*/

private:

  void copy(mpcClusterContentV2& to) const;

private:

  unsigned int fDeadmap;
  unsigned int fWarnmap;
  unsigned int fCutword;



  int fArm;
  int fId;
  int fMultiplicity;
  int fPid;
  //int fSector;
  int fIxpos; //this one is tower w/ max energy
  int fIypos;
  //v2 addition
  int fIxpos_hit; //this one is closest to hit position 
  int fIypos_hit; //not implemented
  //int fType; 



  float fX; //angle corrected log weighted x,y positions
  float fY;
  float fZ;
  float fDx;
  float fDy;
  float fDz;
  float fDispy;
  float fDispx;
  float fLogDispy; //v2 addition
  float fLogDispx; //v2 addition
  float fE;
  float fE9;
  float fEcent;
  float fEcore;
  float fEtofmin;
  float fEtofmax;
  float fChi2;
  float fQuality;
  float fPadispy;
  float fPadispx;
  float fProb_photon;
  float fPhi;
  float fTof;
  float fTofdisp;
  float fTofmin;
  float fTofmax;
  float fTofcorr;
  float fTofcorrmin;
  float fTofcorrmax;
  float fTheta;
  float fLinYcg; // linear cg
  float fLinXcg; // linear cg
  float fLinY; // linear cg w/ angle correction
  float fLinX; // linear cg w/ angle correction
  float fYcgmin; // cg w/ chi2 minimization
  float fXcgmin;  // cg w/ chi2 minimization
  float fYmin;    // cg w/ chi2 minimization w/ angle correction
  float fXmin;  // cg w/ chi2 minimization w/ angle correction
  float fYcg; // log cg
  float fXcg; // log cg
  float fCorrDispy;
  float fCorrDispx;
  float fCorrLogDispy; //v2 addition
  float fCorrLogDispx; //v2 addition
  float fTofHad;
  float fRawTDC;

  float fChi2core;
  int fNdfcore;
  


  bool fIsSplit; //boolean telling whether we tried to split the cluster
  float fX1;
  float fY1;
  float fX2;
  float fY2;
  float fE1;
  float fE2;
  float fChi2_split;
  

  int fNsim; //number of contributors to this cluster.
  short fType; //my own personal scheme for sim particles
    
 
  

/*
  short femcpc3  ;
  short femcpc3neartrk   ;
  float femcpc3dz        ;
  float femcpc3dphi      ;
  short femctrk  ;
  float femctrkdz        ;
  float femctrkdphi      ;
  float fpemctrk ;
  short femctrkquality   ;
*/


  int* fTowerid; //[fMultiplicity]
  
  float* fPartesum; //[fMultiplicity]
  
  int *fIt; //[fNsim]
  int *fID; //[fNsim]
  int *fPri; //[fNsim]
  float *fEdep; //[fNsim] 
  float *fFrac; //[fNsim]
                
                
  int *fKF; //[fNsim]
  int *fNode; //[fNsim]
  
  int *fParentKF; //[fNsim]
  int *fParentID; //[fNsim]



  ClassDef(mpcClusterContentV2,2) // MPC Cluster content version 2
};

#endif

