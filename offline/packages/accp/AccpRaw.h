#ifndef __ACCPRAW_H__
#define __ACCPRAW_H__

#include <iostream>
#include "PHObject.h"
#include "Accp.h"

class AccpRaw : public PHObject //++CINT
{

public:
  virtual ~AccpRaw() {}

  virtual void identify(std::ostream& os = std::cout) const
  {
    os << "virtual AccpRaw object" << std::endl;
    return;
  }

  virtual int isValid() const
  {
    std::cout << "AccpRaw: isValid() not implemented" << std::endl;
    return 0;
  }

  virtual void Reset() {}

  virtual void SetAdcHPost(int ich, int val ) {}
  virtual void SetAdcHPre(int ich, int val ) {}
  virtual void SetAdcLPost(int ich, int val ) {}
  virtual void SetAdcLPre(int ich, int val ) {}

  virtual void SetTdc(int ich, int val ) {}

  virtual int GetAdcHPost(int ich)  const { return -99999;}
  virtual int GetAdcHPre(int ich)  const { return -99999;}
  virtual int GetAdcLPost(int ich)  const { return -99999;}
  virtual int GetAdcLPre(int ich)  const { return -99999;}

  virtual int GetTdc(int ich) const { return -99999;}
 
  ClassDef(AccpRaw,1)    
};

#endif
