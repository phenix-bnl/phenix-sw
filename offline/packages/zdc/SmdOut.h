#ifndef __SMDOUT_H__
#define __SMDOUT_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "ZdcReturncodes.h"

class SmdOut: public PHObject
{
public:

  SmdOut();
  virtual ~SmdOut() {}

  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();
  virtual int isValid() const;

  virtual int AddSmdHit(const float charge, const float time0, const float time1, const int islat);
  virtual float get_Charge(const int iPmt) const;
  virtual float get_Time0(const int iPmt) const;
  virtual float get_Time1(const int iPmt) const;

  virtual void AddSmdNS(const float xpos, const float ypos, const float e, const int arm);
  virtual float get_Xpos(const int arm) const;
  virtual float get_Ypos(const int arm) const;
  virtual float get_Energy(const int arm) const;

  ClassDef(SmdOut,1)
};

#endif	// __SMDOUT_H__
