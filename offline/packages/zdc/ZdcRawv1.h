#ifndef __ZDCRAWV1_H
#define __ZDCRAWV1_H

#include <iostream>
#include "ZdcRaw.h"
#include "TClonesArray.h"

class ZdcRawv1 : public ZdcRaw
{
 public:
  ZdcRawv1();
  virtual ~ZdcRawv1();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  short get_npmt() const { return ZdcRawNpmt; }
  short get_Adc(short iPmt) const;
  short get_Tdc0(short iPmt) const;
  short get_Tdc1(short iPmt) const;
  short get_Tdc(short iPmt, unsigned short itdc) const;

  short AddZdcRawHit(short adc, short tdc0, short tdc1, short ipmt);

protected:
  TClonesArray *GetZdcRawHits() const {return ZdcRawHits;}
  void Clear(Option_t *option = "");

  short ZdcRawNpmt;
  TClonesArray *ZdcRawHits;

  ClassDef(ZdcRawv1,1)
};

#endif


