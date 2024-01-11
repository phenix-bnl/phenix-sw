#ifndef __AERGEAHITS_H__
#define __AERGEAHITS_H__

#include <iostream>
#include "PHObject.h"
#include "AerPISAHit.h"	// c. maguire's aer hit class, see pisa2000/src/root

class AerGeaHits : public PHObject
{
public:
  AerGeaHits();
  virtual ~AerGeaHits() {}

  // accessor methods
  virtual unsigned int get_nhits();
  virtual AerPISAHit *get_AerPISAHits(unsigned int ihit = 0);

  // set methods
  virtual void set_nhits(unsigned int);
  virtual void set_AerPISAHits( AerPISAHit *hitsarray );

  // inherited from PHObject
  virtual void identify(std::ostream& os) const;
  virtual void Reset();
  virtual int isValid() const;

  ClassDef(AerGeaHits,0)  // A Aer hit instance
};

#endif

