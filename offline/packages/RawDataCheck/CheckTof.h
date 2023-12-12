#ifndef __CHECKTOF_H__
#define __CHECKTOF_H__

#include "GranuleCheck.h"

class Event;

class CheckTof: public GranuleCheck
{
 public:
  CheckTof(const char *arm = "EAST");
  virtual ~CheckTof() {}

  int Init();

  int FEMClockCounterCheck(); // two different branches with different fem clk's
  int ResetEvent();

 protected:
  int standard_clock_1;
  int standard_clock_2;
};

#endif /* __CHECKTOF_H__ */
