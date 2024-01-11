#ifndef __T0OUTV1_h
#define __T0OUTV1_h

#include <T0Returncodes.h>
#include <T0Out.h>
#include <iostream>

// bit masks for subsystems, set in int T0List
#define ZDCBIT 0x1
#define BBCBIT 0x2
#define TZRBIT 0x4
#define NTCBIT 0x8

class T0Outv1: public T0Out
{

 public:

  T0Outv1();
  virtual ~T0Outv1() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;

  int set_BbcT0(const float t0, const float t0err);
  int set_NtcT0(const float t0, const float t0err);
  int set_TzrT0(const float t0, const float t0err);
  int set_ZdcT0(const float t0, const float t0err);

  float get_T0() const;
  float get_T0Error() const;

  float get_BbcT0() const {return BbcT0;}
  float get_BbcT0Error() const {return BbcT0Err;}

  float get_NtcT0() const {return NtcT0;}
  float get_NtcT0Error() const {return NtcT0Err;}

  float get_TzrT0() const {return TzrT0;}
  float get_TzrT0Error() const {return TzrT0Err;}

  float get_ZdcT0() const {return ZdcT0;}
  float get_ZdcT0Error() const {return ZdcT0Err;}

  int get_T0List() const {return T0List;}

  bool isBbcT0() const {return T0List&BBCBIT;}
  bool isNtcT0() const {return T0List&NTCBIT;}
  bool isTzrT0() const {return T0List&TZRBIT;}
  bool isZdcT0() const {return T0List&ZDCBIT;}

  int isValid() const {return T0List;}

 protected:
  int ChooseT0() const;  // here we determine the t0 of choice

  int T0List;            // store bits of subsystems with t0 
  float BbcT0;
  float BbcT0Err;
  float NtcT0;
  float NtcT0Err;
  float TzrT0;
  float TzrT0Err;
  float ZdcT0;
  float ZdcT0Err;

  ClassDef(T0Outv1,1)

};

#endif
