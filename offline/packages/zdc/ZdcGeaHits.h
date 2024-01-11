#ifndef __ZDCGEAHITS_H__
#define __ZDCGEAHITS_H__

#include <iostream>
#include "PHObject.h"
#include "ZdcPISAHit.h"	// c. maguire's zdc hit class, see pisa2000/src/root

class ZdcGeaHits : public PHObject
{
public:
  ZdcGeaHits();
  virtual ~ZdcGeaHits() {}

  // accessor methods
  virtual unsigned int get_nhits();
  virtual ZdcPISAHit *get_ZdcPISAHits(unsigned int ihit = 0);

  // set methods
  virtual void set_nhits(unsigned int);
  virtual void set_ZdcPISAHits( ZdcPISAHit *hitsarray );

  // inherited from PHObject
  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();
  virtual int isValid() const;

  ClassDef(ZdcGeaHits,0)  // A Zdc hit instance
};

#endif

