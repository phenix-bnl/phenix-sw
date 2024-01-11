#ifndef ERTOUT_h
#define ERTOUT_h

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "ErtReturncodes.h"

class ErtOut: public PHObject
{
 public:
  virtual ~ErtOut() {}

  /** Virtual copy constructor. Allow to make a copy of the concrete
      object (children of this virtual base class) without knowledge
      of its actual version.
  */
  virtual ErtOut* clone() const { PHOOL_VIRTUAL_WARNING; return 0; }

  virtual void identify(std::ostream& os = std::cout) const 
    {
      os << "virtual ErtOut object";
      return;
    }

  virtual int isValid() const 
    {
      std::cout << "ErtOut: isValid() not implemented" << std::endl;
      return 0;
    }

  virtual int get_ERThit_N() const {return ERT_INVALID_INT;}
  virtual int get_ERTtrigmode(int) const {return ERT_INVALID_INT;}
  virtual int get_ERTarm(int) const {return ERT_INVALID_INT;}
  virtual int get_ERTsector(int) const {return ERT_INVALID_INT;}
  virtual int get_ERTsm(int) const {return ERT_INVALID_INT;}
  virtual int get_ERTbit(int /*trigmode*/, int /*arm*/, int /*sector*/, int /*sm*/) const
  {return ERT_INVALID_INT;}

  virtual int set_ERTbit(int /*trigmode*/, int /*arm*/, int /*sector*/, int /*sm*/)
  {return ERT_INVALID_INT;}

  virtual void Reset(void){return;}

  ClassDef(ErtOut,1)
};
#endif

