#ifndef __LPCRAW_H
#define __LPCRAW_H

#include <iostream>
#include "PHObject.h"
#include "lpcReturncodes.h"

class lpcRaw : public PHObject //++CINT
{
 public:
  virtual ~lpcRaw() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      std::cout << "virtual lpcRaw object" << std::endl;
      return;
    }

  virtual int isValid() const 
    {
      std::cout << "lpcRaw: isValid() not implemented" << std::endl;
      return 0;
    }

  virtual short get_npmt() const {return INVALID_SHORT;}
  virtual short get_Adc(short iPmt)  const {return INVALID_SHORT;}  
  virtual short get_AdcPost(short iPmt)  const {return INVALID_SHORT;}
  virtual short get_AdcPre(short iPmt)  const {return INVALID_SHORT;}
  virtual short get_Tdc0(short iPmt) const {return INVALID_SHORT;}
  virtual short get_Tdc(short iPmt, unsigned short itdc) const {return INVALID_SHORT;}
  virtual short AddlpcRawHit(short adc, short tdc0, short ipmt)  {return INVALID_SHORT;}
  virtual short AddlpcRawHit(short adc_post, short adc_pre, short tdc0, short ipmt)  {return INVALID_SHORT;}

  ClassDef(lpcRaw,1)

};

#endif






