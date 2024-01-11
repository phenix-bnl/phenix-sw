#ifndef __SVXPisaHITV1_H
#define __SVXPisaHITV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxPisaHit.h"
#include "SvxSnglPisaHit.h"

//
//  This class is a so-called "versioned" object.  We are
//  inheriting from the SvxPisaHit.  That virtual base 
//  class has manipulators for SvxPisaHit objects that are
//  not specific to any particular verion of SvxPisaHit objects.
//  This container over-rides the methods of the virtual base class
//  making them specific to version 1 SvxPisaHits.
//
//  *Here* is where we actually make the TClonesArray that holds the
//  single SvxPisaHitv1 objects.
//                                 TKH 8-11-2003
//
//  Created 09/13/2005 by Sasha Lebedev (lebedev@iastate.edu)
//

class SvxPisaHitv1 : public SvxPisaHit
{
 public:
  SvxPisaHitv1();
  SvxPisaHitv1(const SvxPisaHitv1&);
  SvxPisaHitv1& operator=(const SvxPisaHitv1&);
  virtual ~SvxPisaHitv1();

  SvxPisaHit* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the set/get methods...
  void SetnHit      (const unsigned int NPisaHIT) {nSvxPisaHit = NPisaHIT; return;}
  int  GetnHit      () const {return nSvxPisaHit;}

  // Routines to manipulate the particle array...
  int SetTClonesArraySize(const unsigned int ntrk);
  void AddPisaHit          (const unsigned int itrk);
  void RemovePisaHit       (const unsigned int itrk);
  SvxSnglPisaHit* AddPisaHit (const unsigned int itrk, const SvxSnglPisaHit& sngl);
  SvxSnglPisaHit* GetHit(const unsigned int itrk) const;

 protected:
  TClonesArray *GetPisaHit() const {return mySvxPisaHit;}
  unsigned int nSvxPisaHit;
  TClonesArray *mySvxPisaHit;

private:
  // Copy this to dest.
  void copyto(SvxPisaHitv1& dest) const;
  ClassDef(SvxPisaHitv1,1)
};

#endif 






