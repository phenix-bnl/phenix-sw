#ifndef __LPOLRAW_H
#define __LPOLRAW_H

#include <iostream>
#include "PHObject.h"
#include "LPolConst.h"

class LPolRaw : public PHObject //++CINT
{
 public:
  virtual ~LPolRaw() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      std::cout << "virtual LPolRaw object" << std::endl;
      return;
    }

  virtual int isValid() const 
    {
      std::cout << "LPolRaw: isValid() not implemented" << std::endl;
      return 0;
    }

  virtual short get_npmt() const {return INVALID_SHORT;}
  virtual short get_Adc(short iPmt)  const {return INVALID_SHORT;}  
  virtual short get_Tdc0(short iPmt) const {return INVALID_SHORT;}
  virtual short get_Tdc1(short iPmt) const {return INVALID_SHORT;}
  virtual short AddLPolRawHit(short adc, short tdc0, short tdc1, short ipmt)  {return INVALID_SHORT;}

  ClassDef(LPolRaw,1)

};

#endif
