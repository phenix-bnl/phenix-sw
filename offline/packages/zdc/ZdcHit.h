#ifndef __ZDCHIT_H
#define __ZDCHIT_H

#include <iostream>
#include "PHObject.h"

class ZdcHit : public TObject
{
 public:
  ZdcHit() { }
  ZdcHit(float charge, float time0, float time1);
  virtual ~ZdcHit() { }

  float get_Charge() const {return Charge;}
  float get_Time0() const {return Time0;}
  float get_Time1() const {return Time1;}

  void identify(std::ostream& os = std::cout) const;

 protected:
  float Charge;
  float Time0;
  float Time1;

  ClassDef(ZdcHit,1)
};

#endif










