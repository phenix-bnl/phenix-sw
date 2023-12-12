#ifndef CHECKMUTR_H__
#define CHECKMUTR_H__

#include "GranuleCheck.h"

class Event;

class CheckMutr: public GranuleCheck
{
 public:
  CheckMutr(const std::string &arm);
  virtual ~CheckMutr() {}

  int Init();
  int BeginRun(const int runno);
  int FEMClockCounterCheck(); // treat Master and Slave boards different
  int ResetEvent();
  int FEMGL1ClockCounterCheck(); // special to not check multi evt buffering
  int SubsystemCheck(Event *evt, const int iwhat);

 protected:
  int standard_clock[2];
  unsigned int runnumber;
};

#endif /* __CHECKMUTR_H__ */
