#ifndef __MPCCLUSTERCONTENTV1_H__
#define __MPCCLUSTERCONTENTV1_H__

#include "mpcClusterContent.h"

/** (VERSION) mpcClusterContent version 1. 

*/

class mpcClusterContentV1 : public mpcClusterContent
{
public:

  /// CREATORS.
  mpcClusterContentV1();
  mpcClusterContentV1(const mpcClusterContentV1&);
  mpcClusterContentV1& operator=(const mpcClusterContentV1&);

  mpcClusterContentV1* clone(void) const;
  mpcClusterContentV1* create(void) const;

  virtual ~mpcClusterContentV1();
  virtual void Clear(Option_t* option="");

  /// ACCESSORS.

  int arm() const { return fArm; }

  float chi2() const { return fChi2; }

  float corrdispy() const { return fCorrDispy; }
  // the below is now deprecated
  float corrdispz() const { return fCorrDispz; }
  float corrdispx() const { return fCorrDispz; }

  unsigned int cutword() const { return fCutword; }

  unsigned int deadmap() const { return fDeadmap; }
  
  float dispy() const { return fDispy; }
  // the below is now deprecated
  float dispz() const { return fDispz; }
  float dispx() const { return fDispz; }

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
  
  float quality() const { return fQuality; }
  
  int multiplicity() const { return fMultiplicity; }
  
  float padispy() const { return fPadispy; }
  // the blow is deprecated
  float padispz() const { return fPadispz; }
  float padispx() const { return fPadispz; }
  
  // index ranges from 0 to multiplicity().
  float partesum(int index) const;
  
  float prob_photon() const { return fProb_photon; }
  
  float phi() const { return fPhi; }
  
  int pid() const { return fPid; }
  
  void print(std::ostream& out=std::cout) const;

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
  
  float ycg() const { return fYcg; }
  float zcg() const { return fZcg; }

  bool has_yz_cg() const { return true; }
  
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
  void set_corrdisp(float corrdispy, float corrdispx) { fCorrDispy=corrdispy; fCorrDispz=corrdispx; }
  void set_cutword(unsigned int cw) { fCutword=cw; }
  void set_disp(float dispy, float dispx) { fDispy=dispy; fDispz=dispx; }
  void set_dxyz(float dx, float dy, float dz) { fDx=dx; fDy=dy; fDz=dz; }
  void set_e(float e) { fE=e;}
  void set_e9(float e9) { fE9=e9;}
  void set_ecore(float ecore) { fEcore=ecore; }
  void set_ecent(float ecent) { fEcent=ecent; }
  void set_etofmin(float etofmin) { fEtofmin=etofmin; }
  void set_etofmax(float etofmax) { fEtofmax=etofmax; }
  void set_id(int id) { fId=id; }
  void set_ipos(int ix, int iy) { fIxpos=ix; fIypos=iy; }
  void set_quality(float qual) { fQuality=qual; }
  void set_maps(unsigned int dead, unsigned int warn)
  { fDeadmap = dead; fWarnmap=warn; }
  void set_multiplicity(int mul);
  void set_padisp(float padispy, float padispx) 
  { fPadispy=padispy; fPadispz=padispx; }
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
  //void set_type(int type) { fType=type; }
  void set_xyz(float x, float y, float z) { fX=x; fY=y; fZ=z; }
  void set_yz_cg(float ycg, float zcg) { fYcg=ycg; fZcg=zcg; }
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

  void copy(mpcClusterContentV1& to) const;

private:

  unsigned int fDeadmap;
  unsigned int fWarnmap;
  unsigned int fCutword;

  int fArm;
  int fId;
  int fMultiplicity;
  int fPid;
  //int fSector;
  int fIxpos;
  int fIypos;
  //int fType; 

  float fX;
  float fY;
  float fZ;
  float fDx;
  float fDy;
  float fDz;
  float fDispy;
  float fDispz;
  float fE;
  float fE9;
  float fEcent;
  float fEcore;
  float fEtofmin;
  float fEtofmax;
  float fChi2;
  float fQuality;
  float fPadispy;
  float fPadispz;
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
  float fYcg;
  float fZcg;
  float fCorrDispy;
  float fCorrDispz;
  float fTofHad;
  float fRawTDC;
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

  ClassDef(mpcClusterContentV1,1) // MPC Cluster content version 1
};

#endif

