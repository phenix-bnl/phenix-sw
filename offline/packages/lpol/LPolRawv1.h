#ifndef __LPOLRAWV1_H
#define __LPOLRAWV1_H

#include <iostream>
#include "LPolRaw.h"
#include "TClonesArray.h"

class TClonesArray;

class LPolRawv1 : public LPolRaw
{
 public:
  LPolRawv1();
  virtual ~LPolRawv1();

  // from PHObject
  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  short get_npmt() const { return lpolRawNpmt; }
  short get_Adc(short iPmt) const;
  short get_Tdc0(short iPmt) const;
  short get_Tdc1(short iPmt) const;


  short AddLPolRawHit(short adc, short tdc0, short tdc1, short ipmt);

protected:
  TClonesArray *GetLPolRawHits() const {return lpolRawHits;}
  void Clear(Option_t *option = "");

  short lpolRawNpmt;
  TClonesArray *lpolRawHits;

  ClassDef(LPolRawv1,1)
};

#endif
