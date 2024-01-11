#ifndef __LPCRAWHIT_H
#define __LPCRAWHIT_H

#include "PHObject.h"  // okay we get TObject from here inheritance
                       // from TObject via PHObject didn't work for
                       // TClonesArrays at the time of development
#include "lpcReturncodes.h"

/// Contains the raw hits (Adc, Tdc0)
class lpcRawHit : public TObject
{
  
 public:
  lpcRawHit() { }
  virtual ~lpcRawHit() { }
  
  virtual short get_Adc()  const {return INVALID_SHORT;}
  virtual short get_AdcPost()  const {return INVALID_SHORT;}
  virtual short get_AdcPre()  const {return INVALID_SHORT;}
  virtual short get_Tdc0() const {return INVALID_SHORT;}
  
  void identify(std::ostream& os = std::cout) const;
  
  ClassDef(lpcRawHit,1)
};
    
#endif
