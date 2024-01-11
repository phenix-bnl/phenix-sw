#ifndef __effPair_h__
#define __effPair_h__

#include "effParticle.h"
#include "PHObject.h"
#include <iostream>

using namespace std;

class effPair : public PHObject
{
public:

  effPair(const effParticle& particle1, const effParticle& particle2);
  virtual ~effPair();

  double Asymmetry(void) const { return fAsymmetry; }

  int CutMask(void) const { return fCutMask; }

  void identify(ostream& os = cout) const;
  int isValid() const { return 1; }

  double Minv(void) const { return fMinv; }
  double Pt(void) const { return fPt; }
  double Cosine(void) const { return fCosine; }
  double xVertex(void) const { return fxVtx; }
  double AverageImpactAngle(void) const { return fAverageImpactAngle; }
  TLorentzVector PTOT(void) const { return fPTOT; }

  void setCutMask(int mask) { fCutMask=mask; }

  bool isSimulPair(void) const { return fPureSimulPair; }
  void setSimulPair(bool setsimul=true) { fPureSimulPair=setsimul; }

  const effParticle& Particle1(void) const { return fParticle1; }
  const effParticle& Particle2(void) const { return fParticle2; }

  const effParticle& Particle(size_t i) const;

  virtual void Print(Option_t* opt="") const;

private:

  double fMinv;
  double fPt;
  double fAsymmetry;
  double fCosine; // opening angle
  double fxVtx;   // x-vertex
  double fAverageImpactAngle; // average impact angle in EMCal (from vertex) of the particles of the pair
  int fCutMask;
  bool fPureSimulPair;

  effParticle fParticle1;
  effParticle fParticle2;    

  TLorentzVector fPTOT;

  ClassDef(effPair,3) // A pair of EMCAL efficiency-computation clusters
};
#endif
