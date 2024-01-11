#ifndef __LPCRAWV1_H
#define __LPCRAWV1_H

#include <iostream>
#include "lpcRaw.h"

class TClonesArray;

class lpcRawv1 : public lpcRaw
{
 public:
  lpcRawv1();
  virtual ~lpcRawv1();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  short get_npmt() const { return lpcRawNpmt; }
  short get_Adc(short iPmt) const;
  short get_Tdc0(short iPmt) const;
  short get_Tdc(short iPmt, unsigned short itdc) const;

  short AddlpcRawHit(short adc, short tdc0, short ipmt);

protected:
  TClonesArray *GetlpcRawHits() const {return lpcRawHits;}
  void Clear(Option_t *option = "");

  short lpcRawNpmt;
  TClonesArray *lpcRawHits;

  ClassDef(lpcRawv1,1)
};

#endif


