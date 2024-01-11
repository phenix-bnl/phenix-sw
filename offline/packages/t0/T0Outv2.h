#ifndef __T0OUTV2_h
#define __T0OUTV2_h

#include "T0Returncodes.h"
#include "T0Outv1.h"
#include <iostream>

// bit masks for subsystems, set in int T0List
// #define ZDCBIT 0x1
// #define BBCBIT 0x2
// #define TZRBIT 0x4
// #define NTCBIT 0x8
#define FKEBIT 0x10

class T0Outv2: public T0Outv1
{

 public:

  T0Outv2();
  virtual ~T0Outv2() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_FkeT0(const float t0, const float t0err);

  float get_T0() const;
  float get_T0Error() const;

  float get_FkeT0() const {return FkeT0;}
  float get_FkeT0Error() const {return FkeT0Err;}

  int get_T0List() const {return T0List;}

  bool isFkeT0() const {return T0List&FKEBIT;}
  const char *which_t0() const;

 protected:
  int ChooseT0() const;  // here we determine the t0 of choice

  float FkeT0;           // Data members for fake t0
  float FkeT0Err;

  ClassDef(T0Outv2,1)

};

#endif
