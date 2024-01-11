#ifndef _PhCglListv4_Run7a_h
#define _PhCglListv4_Run7a_h

#include <iostream>
#include "PHCentralTrack.h"
#include "PhCglSnglv4_Run7a.h"
#include "PHSnglCentralTrack.h"

class TClonesArray;

class PhCglListv4_Run7a : public PHCentralTrack
{
 public:
  PhCglListv4_Run7a();
  PhCglListv4_Run7a(const PhCglListv4_Run7a&);
  PhCglListv4_Run7a& operator=(const PhCglListv4_Run7a&);
  virtual ~PhCglListv4_Run7a();

  PhCglListv4_Run7a* clone() const;
 
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;
  
  void set_npart (const unsigned int NTRACK) {nCentral = NTRACK; return;}
  unsigned int get_npart() const {return nCentral;}

  void set_eventFilterType (const short trigtype) 
    {evtFilterTypecg = trigtype; return;}
  unsigned short get_eventFilterType () 
    {return evtFilterTypecg;}
  
  
  // Routines to manipulate the particle array
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddPHParticle(const unsigned int itrk);
  PhCglSnglv4_Run7a* AddPHParticle(const PHSnglCentralTrack &track);
 
 protected:
  TClonesArray *GetCentral() const {return Central;}
  unsigned int nCentral;
  unsigned short evtFilterTypecg;

  TClonesArray *Central;

 private:
  // Copy this to dest.
  void copyto(PhCglListv4_Run7a& dest) const;

  ClassDef(PhCglListv4_Run7a,1)
};

#endif
