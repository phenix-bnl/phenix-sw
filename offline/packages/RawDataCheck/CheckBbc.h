#ifndef CHECKBBC_H__
#define CHECKBBC_H__

#include "GranuleCheck.h"

class Event;

class CheckBbc: public GranuleCheck
{
 public:
  CheckBbc(const std::string &name = "BBC");
  virtual ~CheckBbc() {}

  int Init();
  int FEMClockCounterCheck();  // BBC is only 1 packet
  int SubsystemCheck(Event *evt, const int iwhat);  // check for missing packet
  int DoEveryEvent(Event *evt); // check if we have 1 or 2 bbc packets
  int FEMParityErrorCheck();

 protected:

  int twobbcpkts;

};

#endif /* __CHECKBBC_H__ */
