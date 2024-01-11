#ifndef __ZDCNORTHSOUTH_H
#define __ZDCNORTHSOUTH_H

#include <iostream>
#include "PHObject.h"

class ZdcNorthSouth : public TObject
{
 public:
  ZdcNorthSouth() { }
  ZdcNorthSouth(float energy, float timing);
  virtual ~ZdcNorthSouth() { }
  void identify(std::ostream& os = std::cout) const;

  float get_Energy() const {return Energy;}
  float get_Timing() const {return Timing;}

 protected:
  float Energy;
  float Timing;

  ClassDef(ZdcNorthSouth,1)
};

#endif
