#ifndef __effParticle_h__
#define __effParticle_h__

#include "TParticle.h"

// general constants

static const int PDGCODE_ETA = 221;
static const int GEANTCODE_ETA = 17;

static const int PDGCODE_PIZERO = 111;
static const int GEANTCODE_PIZERO = 7;

static const int PDGCODE_GAMMA = 22;
static const int GEANTCODE_GAMMA = 1;

static const int PDGCODE_ELECTRON = 11;
static const int GEANTCODE_ELECTRON = 2;

static const int PDGCODE_POSITRON = -11;
static const int GEANTCODE_POSITRON = 3;


class effParticle : public TParticle
{
public:
  
  effParticle();

  effParticle(Int_t pdg, Int_t status,
	      Int_t mother1, Int_t mother2,
	      Int_t daughter1, Int_t daughter2,
	      Double_t px, Double_t py, Double_t pz, Double_t etot,
	      Double_t vx, Double_t vy, Double_t vz, Double_t time);
  
  effParticle(Int_t pdg, Int_t status,
	      Int_t mother1, Int_t mother2,
	      Int_t daughter1, Int_t daughter2,
	      const TLorentzVector &p,
	      const TLorentzVector &v);

  // WARNING: This is a highly overloaded TParticle object:
  // most of the standard TParticle fields are being "abused" here ...
  
  effParticle(Int_t pdg,  
	      Double_t px, Double_t py, Double_t pz, Double_t etot,
	      double vx, double vy, double vz,
	      double impx,double impy, double impz,
	      double chi2, double tof, bool in_fiducial,
	      int dead, int warn, 
	      int isector, int arm,
	      double impactangle );

  effParticle(const effParticle &part);

  effParticle& effParticle::operator=(const effParticle& part);

  virtual ~effParticle();

  static int geantToPDG(int geantPID);
  
  double Chi2(void) const { return fChi2; }
  double TOF(void) const { return fTOF; }
  
  bool InFiducial(void) const { return fInFiducial; }
  int Sector(void) const { return fSector; }
  int Arm(void) const { return fArm; }

  int Warn(void) const { return fWarn; }
  int Dead(void) const { return fDead; }

  double ImpX(void) const { return fImpX; }
  double ImpY(void) const { return fImpY; }
  double ImpZ(void) const { return fImpZ; }

  double ImpAngle(void) const { return fImpAngle; }

  virtual void Print(Option_t *option = "") const;

private:
  int fDead;
  int fWarn;
  bool fInFiducial;
  double fTOF;
  double fChi2;
  int fSector;
  int fArm;
  double fImpX,fImpY,fImpZ; // impact point (in local coordinates to sector)
  double fImpAngle; // "impact angle" from vertex (in global coordinates !)

  ClassDef(effParticle,3) // Efficiency particle
};

#endif
