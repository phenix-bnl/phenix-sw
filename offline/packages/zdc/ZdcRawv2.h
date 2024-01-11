#ifndef __ZDCRAWV2_H__
#define __ZDCRAWV2_H__

#include <iostream>
#include <cassert>
#include "ZdcRaw.h"

class ZdcRawv2 : public ZdcRaw
{
 public:
  ZdcRawv2() {}
  ZdcRawv2(const Int_t npmt);
  virtual ~ZdcRawv2();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  short get_Adc(short iPmt) const { assert(iPmt<ZdcRawNpmt); return adc[iPmt]; }
  short get_Tdc0(short iPmt) const { assert(iPmt<ZdcRawNpmt); return tdc0[iPmt]; }
  short get_Tdc1(short iPmt) const { assert(iPmt<ZdcRawNpmt); return tdc1[iPmt]; }
  short get_Tdc(short iPmt, unsigned short itdc) const {
    assert(iPmt<ZdcRawNpmt);
    if (itdc==0) return tdc0[iPmt];
    if (itdc==1) return tdc1[iPmt];
    return INVALID_SHORT;
  }

  short AddZdcRawHit(short iadc, short itdc0, short itdc1, short ipmt) {
    assert(ipmt<ZdcRawNpmt);
    adc[ipmt] = iadc;
    tdc0[ipmt] = itdc0;
    tdc1[ipmt] = itdc1;
    return 0;
  }

protected:
  void Clear(Option_t *option = "");

  Int_t   ZdcRawNpmt;
  Short_t *adc;		//[ZdcRawNpmt]
  Short_t *tdc0;	//[ZdcRawNpmt]
  Short_t *tdc1;	//[ZdcRawNpmt]

  ClassDef(ZdcRawv2,1)
};

#endif	// __ZDCRAWV2_H__

