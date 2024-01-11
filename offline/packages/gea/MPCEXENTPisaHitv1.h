#ifndef __MPCEXENTPisaHITV1_H
#define __MPCEXENTPisaHITV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "MPCEXENTPisaHit.h"
#include "MPCEXENTSnglPisaHitv1.h" //must include to satisfy covariance.

class MPCEXENTPisaHitv1 : public MPCEXENTPisaHit
{
 public:
  MPCEXENTPisaHitv1();
  MPCEXENTPisaHitv1(const MPCEXENTPisaHitv1&);
  MPCEXENTPisaHitv1& operator=(const MPCEXENTPisaHitv1&);
  virtual ~MPCEXENTPisaHitv1();

  MPCEXENTPisaHitv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void SetnHit      (const unsigned int NPisaHIT) {nMPCEXENTPisaHit = NPisaHIT; return;}
  int  GetnHit      () const {return nMPCEXENTPisaHit;}

  // Routines to manipulate the particle array...
  int SetTClonesArraySize(const unsigned int ntrk);
  void AddPisaHit          (const unsigned int itrk);
  void RemovePisaHit       (const unsigned int itrk);
  MPCEXENTSnglPisaHitv1* AddPisaHit (const unsigned int itrk, const MPCEXENTSnglPisaHit& sngl);
  MPCEXENTSnglPisaHitv1* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return myMPCEXENTPisaHit;}
  unsigned int nMPCEXENTPisaHit;
  TClonesArray *myMPCEXENTPisaHit;

private:
  // Copy this to dest.
  void copyto(MPCEXENTPisaHitv1& dest) const;
  ClassDef(MPCEXENTPisaHitv1,1)
};

#endif /* __MPCEXENTPisaHITV1_H */






