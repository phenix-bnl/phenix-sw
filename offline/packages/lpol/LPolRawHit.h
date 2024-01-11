#ifndef __LPOLRAWHIT_H
#define __LPOLRAWHIT_H

#include "PHObject.h"  // okay we get TObject from here inheritance
                       // from TObject via PHObject didn't work for
                       // TClonesArrays at the time of development
#include "LPolConst.h"

/// Contains the raw hits (Adc, Tdc0)
class LPolRawHit : public TObject
{
  
 public:
  LPolRawHit() { }
  virtual ~LPolRawHit() { }
  
  virtual short get_Adc()  const {return INVALID_SHORT;}
  //  virtual short get_AdcPost()  const {return INVALID_SHORT;}
  //  virtual short get_AdcPre()  const {return INVALID_SHORT;}
  virtual short get_Tdc0() const {return INVALID_SHORT;}
  virtual short get_Tdc1() const {return INVALID_SHORT;}

  void identify(std::ostream& os = std::cout) const;
  
  ClassDef(LPolRawHit,1)
};
    
#endif
