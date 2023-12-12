#ifndef __CHECKMPC_H__
#define __CHECKMPC_H__

#include <GranuleCheck.h>

class Event;

class CheckMpc: public GranuleCheck
{
 public:
  CheckMpc(const std::string &name = "MPC");
  virtual ~CheckMpc() {}

  int Init();
  int DcmFEMParityErrorCheck();
  int FEMEventSequenceCheck();
  unsigned int LocalEventCounter();
  int GlinkCheck();
  int FEMClockCounterCheck();
  int FEMGL1ClockCounterCheck();

 protected:

};

#endif /* __CHECKMPC_H__ */
