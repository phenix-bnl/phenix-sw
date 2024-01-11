#ifndef __NCCPisaHITV1_H
#define __NCCPisaHITV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "NCCPisaHit.h"
#include "NCCSnglPisaHitv1.h" //must include to satisfy covariance.

class NCCPisaHitv1 : public NCCPisaHit
{
 public:
  NCCPisaHitv1();
  NCCPisaHitv1(const NCCPisaHitv1&);
  NCCPisaHitv1& operator=(const NCCPisaHitv1&);
  virtual ~NCCPisaHitv1();

  NCCPisaHitv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void SetnHit      (const unsigned int NPisaHIT) {nNCCPisaHit = NPisaHIT; return;}
  int  GetnHit      () const {return nNCCPisaHit;}

  // Routines to manipulate the particle array...
  int SetTClonesArraySize(const unsigned int ntrk);
  void AddPisaHit          (const unsigned int itrk);
  void RemovePisaHit       (const unsigned int itrk);
  NCCSnglPisaHitv1* AddPisaHit (const unsigned int itrk, const NCCSnglPisaHit& sngl);
  NCCSnglPisaHitv1* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return myNCCPisaHit;}
  unsigned int nNCCPisaHit;
  TClonesArray *myNCCPisaHit;

private:
  // Copy this to dest.
  void copyto(NCCPisaHitv1& dest) const;
  ClassDef(NCCPisaHitv1,1)
};

#endif /* __NCCPisaHITV1_H */






