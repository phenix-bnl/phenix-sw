#ifndef __EMCEMBEDEVENTV1_H__
#define __EMCEMBEDEVENTV1_H__

#include "emcEmbedEvent.h"

class emcEmbedEventv1 : public emcEmbedEvent
{
 public:

  emcEmbedEventv1();
  virtual ~emcEmbedEventv1();

  emcEmbedEventv1* clone() const { return new emcEmbedEventv1(*this); }

  double energy(unsigned int) const { return fEnergy; }

  double mass(unsigned int) const { return fMass; }

  double momentum(unsigned int) const { return fMomentum; }

  int pid(unsigned int) const { return fPid; }

  double pt(unsigned int) const { return fPt; }

  double px(unsigned int) const { return fPx; }

  double py(unsigned int) const { return fPy; }

  double pz(unsigned int) const { return fPz; }
 
  void identify(std::ostream& os = std::cout) const;

  int isValid() const { return fIsValid; }

  void print(std::ostream& os = std::cout) const;

  void Reset();

  void set_primary(unsigned int,
		   int pid, double energy, 
		   double px, double py, double pz,
		   double zreal, double zsimu);

  size_t size() const { return fSize; }

  double zreal(unsigned int) const { return fZreal; }

  double zsimu(unsigned int) const { return fZsimu; }

 private:

  double fEnergy;
  double fMass;
  double fMomentum;
  int fPid;
  double fPt;
  double fPx;
  double fPy;
  double fPz;
  int fIsValid;
  unsigned int fSize;
  double fZreal;
  double fZsimu;

  ClassDef(emcEmbedEventv1,1)
};

#endif
