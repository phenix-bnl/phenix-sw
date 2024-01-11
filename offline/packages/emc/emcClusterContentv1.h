#ifndef __emcClusterContentv1_h__
#define __emcClusterContentv1_h__

#include "emcClusterContent.h"

/** (VERSION) First version of emcClusterContent. */

class emcClusterContentv1 : public emcClusterContent
{
public:

  /// CREATORS.
  emcClusterContentv1();
  emcClusterContentv1(const emcClusterContentv1&);
  emcClusterContentv1& operator=(const emcClusterContentv1&);

  emcClusterContentv1* clone(void) const;
  emcClusterContentv1* create(void) const;

  virtual ~emcClusterContentv1();
  virtual void Clear(Option_t* option="");

  /// ACCESSORS.

  int arm() const { return fArm; }

  float chi2() const {return fChi2; }
  
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
  
  int isValid() const;

  int iypos() const { return fIypos; }
  int izpos() const { return fIzpos; }
  
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
  
  int sector() const { return fSector; }
  
  float tof() const { return fTof; }
  float tofdisp() const { return fTofdisp; }
  float tofmin() const { return fTofmin; }
  float tofmax() const { return fTofmax; }
 
  float tofcorr() const { return fTofcorr; }
  float tofcorrmin() const { return fTofcorrmin; }
  float tofcorrmax() const { return fTofcorrmax; }
  
  float theta() const { return fTheta; }
  
  int towerid(int index) const;
  
  int type() const { return fType; }
  
  unsigned int warnmap() const { return fWarnmap; }
  
  float x() const { return fX; }  
  float y() const { return fY; }
  float z() const { return fZ; }
  
  /// MUTATORS.
  
  void Reset();

  void set_arm(int arm) { fArm=arm; }
  void set_chi2(float chi2) { fChi2=chi2; }
  void set_disp(float dispy, float dispz) { fDispy=dispy; fDispz=dispz; }
  void set_dxyz(float dx, float dy, float dz) { fDx=dx; fDy=dy; fDz=dz; }
  void set_e(float e) { fE=e;}
  void set_e9(float e9) { fE9=e9;}
  void set_ecore(float ecore) { fEcore=ecore; }
  void set_ecent(float ecent) { fEcent=ecent; }
  void set_etofmin(float etofmin) { fEtofmin=etofmin; }
  void set_etofmax(float etofmax) { fEtofmax=etofmax; }
  void set_id(int id) { fId=id; }
  void set_ipos(int iy, int iz) { fIypos=iy; fIzpos=iz; }
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
  void set_sector(int sector) { fSector=sector; }
  void set_tof(float tof) { fTof=tof; }
  void set_tofdisp(float tofdisp) { fTofdisp=tofdisp; }
  void set_tofmin(float tofmin) { fTofmin=tofmin; }
  void set_tofmax(float tofmax) { fTofmax=tofmax; }
  void set_tofcorr(float tofcorr) { fTofcorr=tofcorr; }
  void set_tofcorrmin(float tofcorrmin) { fTofcorrmin=tofcorrmin; }
  void set_tofcorrmax(float tofcorrmax) { fTofcorrmax=tofcorrmax; }
  void set_theta(float theta) { fTheta=theta;}
  void set_towerid(int index, int value);
  void set_type(int type) { fType=type; }
  void set_xyz(float x, float y, float z) { fX=x; fY=y; fZ=z; }

private:

  void copy(emcClusterContentv1& to) const;

private:

  unsigned int fDeadmap;
  unsigned int fWarnmap;

  int fArm;
  int fId;
  int fMultiplicity;
  int fPid;
  int fSector;
  int fIypos;
  int fIzpos;
  int fType; 

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

  int* fTowerid; //[fMultiplicity]

  float* fPartesum; //[fMultiplicity]

  ClassDef(emcClusterContentv1,3) // EMCAL Cluster content version 1.
};

#endif
