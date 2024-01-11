#ifndef __BBCRAWHIT_H
#define __BBCRAWHIT_H

#include "TObject.h"
#include <iostream>

class BbcRawHit : public TObject
{

 public:
  BbcRawHit() { }
  BbcRawHit(const short pmt, const short adc, 
            const short tdc0, const short tdc1);
  virtual ~BbcRawHit() { }

  short get_Pmt() const {return Pmt;}
  short get_Adc() const {return Adc;}
  short get_Tdc0() const {return Tdc0;}
  short get_Tdc1() const {return Tdc1;}

  void identify(std::ostream& os = std::cout) const;

 protected:
  short Pmt;
  short Adc;
  short Tdc0;
  short Tdc1;

  ClassDef(BbcRawHit,1)
};

#endif
