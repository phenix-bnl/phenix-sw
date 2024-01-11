#ifndef __EMCCLUSTERCONTENTV6_H__
#define __EMCCLUSTERCONTENTV6_H__

#include "emcClusterContent.h"

/** (VERSION) emcClusterContent version 6. 

- This one has adc() and amutac() value for the central tower.
- cutword() ditched.
- version() added.
- Type, arm, sector, iypos, izpos are not stored any more but 
reconstructed from towerid[0]
- tofcorr(), tofcorrmin(), tofcorrmax() became obsolate and
returns the same as tof(), tofmin() or tofmax().

*/

class emcClusterContentv6 : public emcClusterContent
{
public:

  /// CREATORS.
  emcClusterContentv6();
  emcClusterContentv6(const emcClusterContentv6&);
  emcClusterContentv6& operator=(const emcClusterContentv6&);

  emcClusterContentv6* clone(void) const;
  emcClusterContentv6* create(void) const;

  virtual ~emcClusterContentv6();
  virtual void Clear(Option_t* option="");

  /// ACCESSORS.

  float chi2() const { return fChi2; }

  float corrdispy() const { return fCorrDispy; }
  float corrdispz() const { return fCorrDispz; }

  unsigned int deadmap() const { return fDeadmap; }
  
  float dispy() const { return fDispy; }
  float dispz() const { return fDispz; }

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

  int version() const { return 6; }

  int isValid() const;

  float quality() const { return fQuality; }
  
  int multiplicity() const { return fMultiplicity; }
  
  float padispy() const { return fPadispy; }
  float padispz() const { return fPadispz; }
  
  // index ranges from 0 to multiplicity().
  float partesum(int index) const;
  
  float prob_photon() const { return fProb_photon; }
  
  float phi() const { return fPhi; }
  
  int pid() const { return fPid; }
  
  void print(std::ostream& out=std::cout) const;

  float rawtdc() const { return fRawTDC; }
  bool has_rawtdc() const { return true; }

  float adc() const { return fADC; }
  bool has_adc() const { return true; }

  short amutac() const { return fAMUTAC; }
  bool has_amutac() const { return true; }

  float simfrac() const { return 0; }

  float tof() const { return fTof; }
  float tofhad() const { return fTofHad; }
  float tofdisp() const { return fTofdisp; }
  float tofmin() const { return fTofmin; }
  float tofmax() const { return fTofmax; }
 
  float tofcorr() const { return tof(); } // TofCorr obsolete
  float tofcorrmin() const { return tofmin(); } // TofCorrmin obsolete
  float tofcorrmax() const { return tofmax(); } // TofCorrmax obsolete
  
  float theta() const { return fTheta; }
  
  int towerid(int index) const;
  
  int type() const;
  int sector() const;
  int arm() const;
  int iypos() const;
  int izpos() const;

  unsigned int warnmap() const { return fWarnmap; }
  
  float x() const { return fX; }  
  float y() const { return fY; }
  float z() const { return fZ; }
  
  float ycg() const { return fYcg; }
  float zcg() const { return fZcg; }

  bool has_yz_cg() const { return true; }
  
  short emcpc3       () const { return femcpc3        ;}
  short emcpc3neartrk() const { return femcpc3neartrk ;}
  float emcpc3dz     () const { return femcpc3dz      ;}
  float emcpc3dphi   () const { return femcpc3dphi    ;}
  short emctrk       () const { return femctrk        ;}
  float emctrkdz     () const { return femctrkdz      ;}
  float emctrkdphi   () const { return femctrkdphi    ;}
  float pemctrk      () const { return fpemctrk       ;}
  short emctrkquality() const { return femctrkquality ;}




  /// MUTATORS.
  
  void Reset();

  void set_chi2(float chi2) { fChi2=chi2; }
  void set_corrdisp(float corrdispy, float corrdispz) { fCorrDispy=corrdispy; fCorrDispz=corrdispz; }
  void set_disp(float dispy, float dispz) { fDispy=dispy; fDispz=dispz; }
  void set_dxyz(float dx, float dy, float dz) { fDx=dx; fDy=dy; fDz=dz; }
  void set_e(float e) { fE=e;}
  void set_e9(float e9) { fE9=e9;}
  void set_ecore(float ecore) { fEcore=ecore; }
  void set_ecent(float ecent) { fEcent=ecent; }
  void set_etofmin(float etofmin) { fEtofmin=etofmin; }
  void set_etofmax(float etofmax) { fEtofmax=etofmax; }
  void set_id(int id) { fId=id; }
  void set_quality(float qual) { fQuality=qual; }
  void set_maps(unsigned int dead, unsigned int warn)
  { fDeadmap = dead; fWarnmap=warn; }
  void set_multiplicity(int mul);
  void set_padisp(float padispy, float padispz) 
  { fPadispy=padispy; fPadispz=padispz; }
  void set_partesum(int index, float value);
  void set_prob_photon(float prob) { fProb_photon = prob; }
  void set_phi(float phi) { fPhi=phi; }
  void set_pid(int pid) { fPid=pid; }
  void set_rawtdc(float rtdc) { fRawTDC=rtdc; }
  void set_adc(float adc) { fADC=adc; }
  void set_amutac(short amutac) { fAMUTAC=amutac; }
  void set_tof(float tof) { fTof=tof; }  
  void set_tofhad(float tofhad) { fTofHad=tofhad; }
  void set_tofdisp(float tofdisp) { fTofdisp=tofdisp; }
  void set_tofmin(float tofmin) { fTofmin=tofmin; }
  void set_tofmax(float tofmax) { fTofmax=tofmax; }
  void set_tofcorr(float tofcorr) { set_tof(tofcorr); } // obsolete
  void set_tofcorrmin(float tofcorrmin) { set_tofmin(tofcorrmin); } // obsolete
  void set_tofcorrmax(float tofcorrmax) { set_tofmax(tofcorrmax); } // obsolete
  void set_theta(float theta) { fTheta=theta;}
  void set_towerid(int index, int value);
  void set_xyz(float x, float y, float z) { fX=x; fY=y; fZ=z; }
  void set_yz_cg(float ycg, float zcg) { fYcg=ycg; fZcg=zcg; }
  void set_emcpc3       (short val) {femcpc3       = val;}
  void set_emcpc3neartrk(short val) {femcpc3neartrk= val;}
  void set_emcpc3dz     (float val) {femcpc3dz     = val;}
  void set_emcpc3dphi   (float val) {femcpc3dphi   = val;}
  void set_emctrk       (short val) {femctrk       = val;}
  void set_emctrkdz     (float val) {femctrkdz     = val;}
  void set_emctrkdphi   (float val) {femctrkdphi   = val;}
  void set_pemctrk      (float val) {fpemctrk      = val;}
  void set_emctrkquality(short val) {femctrkquality= val;}

private:

  void copy(emcClusterContentv6& to) const;

private:

  unsigned int fDeadmap;
  unsigned int fWarnmap;

  int fId;
  int fMultiplicity;
  int fPid;

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
  float fTheta;
  float fYcg;
  float fZcg;
  float fCorrDispy;
  float fCorrDispz;
  float fTofHad;
  float femcpc3dz;
  float femcpc3dphi;
  float femctrkdz;
  float femctrkdphi;
  float fpemctrk;
  float fRawTDC;
  float fADC;
  short femcpc3;
  short femcpc3neartrk;
  short femctrk;
  short femctrkquality;
  short fAMUTAC;

  int* fTowerid; //[fMultiplicity]

  float* fPartesum; //[fMultiplicity]

  ClassDef(emcClusterContentv6,1) // EMCAL Cluster content version 6
};

#endif

