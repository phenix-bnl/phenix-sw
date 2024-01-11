#ifndef _PhPhotonSnglv2_h_
#define _PhPhotonSnglv2_h_

#include "emcClusterContent.h"

class PhPhotonSnglv2 : public emcClusterContent{
  
 public:
  PhPhotonSnglv2();
  PhPhotonSnglv2(const PhPhotonSnglv2&);
  PhPhotonSnglv2& operator=(const PhPhotonSnglv2&);

  PhPhotonSnglv2* clone(void) const;
  PhPhotonSnglv2* create(void) const;

  virtual ~PhPhotonSnglv2();
  virtual void Clear(Option_t* option="");
  void identify(std::ostream& os=std::cout) const;
  int isValid() const;
  void print(std::ostream& out=std::cout) const;
  void Reset();

  int arm() const { return fArm; }
  int sector() const { return fSector; }
  float chi2() const { return fChi2; }
  float dispy() const { return fDispy; }
  float dispz() const { return fDispz; }
  float padispy() const { return fPadispy; }
  float padispz() const { return fPadispz; }
  float corrdispy() const { return fCorrDispy; }
  float corrdispz() const { return fCorrDispz; }
  unsigned int deadmap() const { return fDeadmap; }
  unsigned int warnmap() const { return fWarnmap; }  
  float e() const { return fE; }
  float ecore() const { return fEcore; }
  float ecent() const { return fEcent; }
  float rawtdc() const { return fRawTDC; }
  int iypos() const { return fIypos; }
  int izpos() const { return fIzpos; }
  float prob_photon() const { return fProb_photon; }  
  float tof() const { return fTof; }
  float tofcorr() const { return fTofcorr; }
  float x() const { return fX; }  
  float y() const { return fY; }
  float z() const { return fZ; }
  float ycg() const { return fYcg; }
  float zcg() const { return fZcg; }
  float emcpc3dz     () const { return femcpc3dz      ;}
  float emcpc3dphi   () const { return femcpc3dphi    ;}
  float emctrkdz     () const { return femctrkdz      ;}
  float emctrkdphi   () const { return femctrkdphi    ;}
  int multiplicity() const { return fMultiplicity; }  
  // index ranges from 0 to multiplicity().
  float partesum(int index) const;
  int towerid(int index) const;  

  void set_arm(int arm) { fArm=arm; }
  void set_sector(int sector) { fSector=sector; }
  void set_chi2(float chi2) { fChi2=chi2; }
  void set_disp(float dispy, float dispz) { fDispy=dispy; fDispz=dispz; }
  void set_padisp(float padispy, float padispz) { fPadispy=padispy; fPadispz=padispz; }
  void set_corrdisp(float corrdispy, float corrdispz) { fCorrDispy=corrdispy; fCorrDispz=corrdispz; }
  void set_maps(unsigned int dead, unsigned int warn) { fDeadmap = dead; fWarnmap=warn; }
  void set_e(float e) { fE=e;}
  void set_ecore(float ecore) { fEcore=ecore; }
  void set_ecent(float ecent) { fEcent=ecent; }
  void set_rawtdc(float rawtdc) { fRawTDC=rawtdc; }
  void set_ipos(int iy, int iz) { fIypos=iy; fIzpos=iz; }
  void set_prob_photon(float prob) { fProb_photon = prob; }
  void set_tof(float tof) { fTof=tof; }  
  void set_tofcorr(float tofcorr) { fTofcorr=tofcorr; }
  void set_xyz(float x, float y, float z) { fX=x; fY=y; fZ=z; }
  void set_yz_cg(float ycg, float zcg) { fYcg=ycg; fZcg=zcg; }
  void set_emcpc3dz     (float val) {femcpc3dz     = val;}
  void set_emcpc3dphi   (float val) {femcpc3dphi   = val;}
  void set_emctrkdz     (float val) {femctrkdz     = val;}
  void set_emctrkdphi   (float val) {femctrkdphi   = val;}  
  void set_multiplicity(int mul);
  void set_partesum(int index, float value);
  void set_towerid(int index, int value);

  // nDST variables not stored in pDST:
  bool has_id() const { return false; }
  bool has_pid() const { return false; }
  bool has_type() const { return false; }
  bool has_Dxyz() const { return false; }
  bool has_E9() const { return false; }
  bool has_Etofmin() const { return false; }
  bool has_Etofmax() const { return false; }
  bool has_Quality() const { return false; }
  bool has_Phi() const { return false; }
  bool has_Theta() const { return false; }
  bool has_Tofdisp() const { return false; }
  bool has_Tofmin() const { return false; }
  bool has_Tofmax() const { return false; }
  bool has_Tofcorrmin() const { return false; }
  bool has_Tofcorrmax() const { return false; }
  //kensuke's needs this to recalibrate the emc tof
  //  bool has_rawtdc() const { return false; } 

private:

  void copy(PhPhotonSnglv2& to) const;

private:

  unsigned int fDeadmap;
  unsigned int fWarnmap;

  int fArm;
  int fMultiplicity;
  int fSector;
  int fIypos;
  int fIzpos;

  float fX;
  float fY;
  float fZ;
  float fDispy;
  float fDispz;
  float fE;
  float fEcent;
  float fRawTDC;
  float fEcore;
  float fChi2;
  float fPadispy;
  float fPadispz;
  float fProb_photon;
  float fTof;
  float fTofcorr;
  float fYcg;
  float fZcg;
  float fCorrDispy;
  float fCorrDispz;
  float femcpc3dz;
  float femcpc3dphi;
  float femctrkdz;
  float femctrkdphi;
  
  int* fTowerid; //[fMultiplicity]
  float* fPartesum; //[fMultiplicity]
 
  ClassDef(PhPhotonSnglv2,1)
};

#endif
