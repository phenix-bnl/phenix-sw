#ifndef _PhCglListv3_h
#define _PhCglListv3_h

#include <iostream>
#include "PHCentralTrack.h"

class TClonesArray;

class PhCglListv3 : public PHCentralTrack
{
 public:
  PhCglListv3();
  PhCglListv3(const PhCglListv3&);
  PhCglListv3& operator=(const PhCglListv3&);
  virtual ~PhCglListv3();

  PhCglListv3* clone() const;
 
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
  void copyto(PhCglListv3& dest) const;

  ClassDef(PhCglListv3,1)
};

#endif
