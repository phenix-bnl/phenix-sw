#ifndef CHECKMUTRIG_H__
#define CHECKMUTRIG_H__

#include "GranuleCheck.h"

class CheckMutrig: public GranuleCheck
{
 public:
  CheckMutrig(const std::string &system);
  virtual ~CheckMutrig() {}

  int Init();

  int DcmFEMParityErrorCheck(); // Dcm does not recalculte fem parity
  unsigned int LocalEventCounter();
  int FEMEventSequenceCheck();
  int FEMGL1ClockCounterCheck();
  int FEMClockCounterCheck();
  int GlinkCheck();

 protected:
  CheckMutrig() {}
};

#endif /* __CHECKMUTRIG_H__ */
