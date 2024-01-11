#ifndef USEINT_H
#define USEINT_H

#include <half/half.h>

class useInt
{
  
 public:
  
  useInt();
  virtual ~useInt();
  
  union floatint
  {
    float    f32;
    int      i32;
  };
  
static float GetFloat(const int ival)
  {
    floatint fi;
    fi.i32 = ival;
    return fi.f32;
  }

static float GetFloat(const short int ival)
  {
    half halfvar;
    halfvar.setBits(ival);
    return halfvar;
  }

};


#endif
