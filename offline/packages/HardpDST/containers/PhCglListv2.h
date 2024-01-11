#ifndef _PhCglListv2_h
#define _PhCglListv2_h

#include <iostream>
#include "PHCentralTrack.h"

class TClonesArray;

class PhCglListv2 : public PHCentralTrack
{
 public:
  PhCglListv2();
  PhCglListv2(const PhCglListv2&);
  PhCglListv2& operator=(const PhCglListv2&);
  virtual ~PhCglListv2();

  PhCglListv2* clone() const;
 
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  
  void set_npart (const unsigned int NTRACK) {nCentral = NTRACK; return;}
  unsigned int get_npart() const {return nCentral;}
  
  // Routines to manipulate the particle array
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddPHParticle      (const unsigned int itrk);
 
 protected:
  TClonesArray *GetCentral() const {return Central;}
  unsigned int nCentral;
  TClonesArray *Central;

 private:
  // Copy this to dest.
  void copyto(PhCglListv2& dest) const;

  ClassDef(PhCglListv2,1)
};

#endif
