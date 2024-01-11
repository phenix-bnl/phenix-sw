#ifndef __NCCPisaHITV2_H
#define __NCCPisaHITV2_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "NCCPisaHit.h"
#include "NCCSnglPisaHitv2.h" //must include to satisfy covariance.

class NCCPisaHitv2 : public NCCPisaHit
{
 public:
  NCCPisaHitv2();
  NCCPisaHitv2(const NCCPisaHitv2&);
  NCCPisaHitv2& operator=(const NCCPisaHitv2&);
  virtual ~NCCPisaHitv2();

  NCCPisaHitv2* clone() const;

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
  NCCSnglPisaHitv2* AddPisaHit (const unsigned int itrk, const NCCSnglPisaHit& sngl);
  NCCSnglPisaHitv2* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return myNCCPisaHit;}
  unsigned int nNCCPisaHit;
  TClonesArray *myNCCPisaHit;

private:
  // Copy this to dest.
  void copyto(NCCPisaHitv2& dest) const;
  ClassDef(NCCPisaHitv2,1)
};

#endif /* __NCCPisaHITV2_H */






