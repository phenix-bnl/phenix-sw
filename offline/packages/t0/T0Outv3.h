#ifndef __T0OUTV3_h
#define __T0OUTV3_h

#include <T0Returncodes.h>
#include <T0Outv2.h>
#include <iostream>

// bit masks for subsystems, set in int T0List
// #define ZDCBIT 0x1
// #define BBCBIT 0x2
// #define TZRBIT 0x4
// #define NTCBIT 0x8
// #define FKEBIT 0x10
#define NTCPBIT 0x20

class T0Outv3: public T0Outv2
{

 public:

  T0Outv3();
  virtual ~T0Outv3() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_NtcpT0(const float t0, const float t0err);

  float get_T0() const;
  float get_T0Error() const;

  float get_NtcpT0() const {return FkeT0;}
  float getNtcpT0Error() const {return FkeT0Err;}

  int get_T0List() const {return T0List;}

  bool isNtcpT0() const {return T0List&NTCPBIT;}
  const char *which_t0() const;

 protected:
  int ChooseT0() const;   // here we determine the t0 of choice

  float NtcpT0;           // Data members for Ntcp t0
  float NtcpT0Err;

  ClassDef(T0Outv3,1)

};

#endif
