#ifndef __SMDOUTV1_H__
#define __SMDOUTV1_H__

#include "SmdOut.h"
#include "ZdcReturncodes.h"

#include <TClonesArray.h>

#include <iostream>

class SmdOutv1: public SmdOut
{
 public:

  SmdOutv1();
  virtual ~SmdOutv1();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  int AddSmdHit(const float charge, const float time0, const float time1, const int islat);
  float get_Charge(const int iPmt) const;
  float get_Time0(const int iPmt) const;
  float get_Time1(const int iPmt) const;

  void AddSmdNS(const float xpos, const float ypos, const float e, const int arm);
  float get_Xpos(const int arm) const;
  float get_Ypos(const int arm) const;
  float get_Energy(const int arm) const;

protected:
  TClonesArray *GetSmdHits() const {return SmdHits;}
  TClonesArray *GetSmdNS() const {return SmdNS;}
  void Clear(Option_t *option = "");

  TClonesArray *SmdHits;	// slat information
  TClonesArray *SmdNS;		// finished information

  ClassDef(SmdOutv1,1)
};

#endif	// __SMDOUTV1_H__

