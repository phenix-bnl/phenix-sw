#ifndef __SMDHIT_H__
#define __SMDHIT_H__

#include <iostream>
#include "PHObject.h"

class SmdHit : public TObject
{
 public:
  SmdHit() { }
  SmdHit(float charge, float time0, float time1);
  virtual ~SmdHit() { }

  float get_Charge() const {return Charge;}
  float get_Time0() const {return Time0;}
  float get_Time1() const {return Time1;}

  void identify(std::ostream& os = std::cout) const;

 protected:
  float Charge;
  float Time0;
  float Time1;

  ClassDef(SmdHit,1)
};

#endif	// __SMDHIT_H__

