#ifndef __ZDCRAW_H
#define __ZDCRAW_H

#include <iostream>
#include "PHObject.h"
#include "ZdcReturncodes.h"

class ZdcRaw : public PHObject //++CINT
{
 public:
  virtual ~ZdcRaw() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      std::cout << "virtual ZdcRaw object" << std::endl;
      return;
    }

  virtual int isValid() const 
    {
      std::cout << "ZdcRaw: isValid() not implemented" << std::endl;
      return 0;
    }

  virtual short get_npmt() const {return INVALID_SHORT;}
  virtual short get_Adc(short iPmt)  const {return INVALID_SHORT;}
  virtual short get_Tdc0(short iPmt) const {return INVALID_SHORT;}
  virtual short get_Tdc1(short iPmt) const {return INVALID_SHORT;}
  virtual short get_Tdc(short iPmt, unsigned short itdc) const {return INVALID_SHORT;}
  virtual short AddZdcRawHit(short adc, short tdc0, short tdc1, short ipmt)  {return INVALID_SHORT;}

  ClassDef(ZdcRaw,1)

};

#endif
