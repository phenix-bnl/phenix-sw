#ifndef __LPCRAWV2_H
#define __LPCRAWV2_H

#include <iostream>
#include "lpcRaw.h"

class TClonesArray;

class lpcRawv2 : public lpcRaw
{
 public:
  lpcRawv2();
  virtual ~lpcRawv2();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  short get_npmt() const { return lpcRawNpmt; }
  short get_AdcPost(short iPmt) const;
  short get_AdcPre(short iPmt) const;
  short get_Tdc0(short iPmt) const;
  short get_Tdc(short iPmt, unsigned short itdc) const;

  short AddlpcRawHit(short adc_post, short adc_pre, short tdc0, short ipmt);

protected:
  TClonesArray *GetlpcRawHits() const {return lpcRawHits;}
  void Clear(Option_t *option = "");

  short lpcRawNpmt;
  TClonesArray *lpcRawHits;

  ClassDef(lpcRawv2,1)
};

#endif


