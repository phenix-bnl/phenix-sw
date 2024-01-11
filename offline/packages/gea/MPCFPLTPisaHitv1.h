#ifndef __MPCFPLTPisaHITV1_H
#define __MPCFPLTPisaHITV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "MPCFPLTPisaHit.h"
#include "MPCFPLTSnglPisaHitv1.h" //must include to satisfy covariance.

class MPCFPLTPisaHitv1 : public MPCFPLTPisaHit
{
 public:
  MPCFPLTPisaHitv1();
  MPCFPLTPisaHitv1(const MPCFPLTPisaHitv1&);
  MPCFPLTPisaHitv1& operator=(const MPCFPLTPisaHitv1&);
  virtual ~MPCFPLTPisaHitv1();

  MPCFPLTPisaHitv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void SetnHit      (const unsigned int NPisaHIT) {nMPCFPLTPisaHit = NPisaHIT; return;}
  int  GetnHit      () const {return nMPCFPLTPisaHit;}

  // Routines to manipulate the particle array...
  int SetTClonesArraySize(const unsigned int ntrk);
  void AddPisaHit          (const unsigned int itrk);
  void RemovePisaHit       (const unsigned int itrk);
  MPCFPLTSnglPisaHitv1* AddPisaHit (const unsigned int itrk, const MPCFPLTSnglPisaHit& sngl);
  MPCFPLTSnglPisaHitv1* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return myMPCFPLTPisaHit;}
  unsigned int nMPCFPLTPisaHit;
  TClonesArray *myMPCFPLTPisaHit;

private:
  // Copy this to dest.
  void copyto(MPCFPLTPisaHitv1& dest) const;
  ClassDef(MPCFPLTPisaHitv1,1)
};

#endif /* __MPCFPLTPisaHITV1_H */






