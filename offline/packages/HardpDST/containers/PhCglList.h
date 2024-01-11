#ifndef _PhCglList_h
#define _PhCglList_h

#include <iostream>
#include "PHCentralTrack.h"

class TClonesArray;

class PhCglList : public PHCentralTrack
{
 public:
  PhCglList();
  virtual ~PhCglList();

  void Reset();
  void identify(std::ostream &os=std::cout) const;

  void set_npart (const unsigned int NTRACK) {nCentral = NTRACK; return;}
  unsigned int get_npart() const {return nCentral;}

 protected:
  TClonesArray *GetCentral() const {return Central;}
  unsigned int nCentral;
  TClonesArray *Central;

  ClassDef(PhCglList,1)
};

#endif
