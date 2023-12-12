#ifndef CHECKZDC_H__
#define CHECKZDC_H__

#include "GranuleCheck.h"

class Event;

class CheckZdc: public GranuleCheck
{
 public:
  CheckZdc();
  virtual ~CheckZdc() {}

  int Init();

  int FEMClockCounterCheck(); // for single FEM pointless
  int SubsystemCheck(Event *evt, const int iwhat);  // check for missing packet

 protected:

};

#endif /* __CHECKZDC_H__ */
