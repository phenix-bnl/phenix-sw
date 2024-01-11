#ifndef __MPCEXABSPisaHITV1_H
#define __MPCEXABSPisaHITV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "MPCEXABSPisaHit.h"
#include "MPCEXABSSnglPisaHitv1.h" //must include to satisfy covariance.

class MPCEXABSPisaHitv1 : public MPCEXABSPisaHit
{
 public:
  MPCEXABSPisaHitv1();
  MPCEXABSPisaHitv1(const MPCEXABSPisaHitv1&);
  MPCEXABSPisaHitv1& operator=(const MPCEXABSPisaHitv1&);
  virtual ~MPCEXABSPisaHitv1();

  MPCEXABSPisaHitv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void SetnHit      (const unsigned int NPisaHIT) {nMPCEXABSPisaHit = NPisaHIT; return;}
  int  GetnHit      () const {return nMPCEXABSPisaHit;}

  // Routines to manipulate the particle array...
  int SetTClonesArraySize(const unsigned int ntrk);
  void AddPisaHit          (const unsigned int itrk);
  void RemovePisaHit       (const unsigned int itrk);
  MPCEXABSSnglPisaHitv1* AddPisaHit (const unsigned int itrk, const MPCEXABSSnglPisaHit& sngl);
  MPCEXABSSnglPisaHitv1* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return myMPCEXABSPisaHit;}
  unsigned int nMPCEXABSPisaHit;
  TClonesArray *myMPCEXABSPisaHit;

private:
  // Copy this to dest.
  void copyto(MPCEXABSPisaHitv1& dest) const;
  ClassDef(MPCEXABSPisaHitv1,1)
};

#endif /* __MPCEXABSPisaHITV1_H */






