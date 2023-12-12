#ifndef CHECKMUID_H__
#define CHECKMUID_H__

#include "GranuleCheck.h"

class Event;

class CheckMuid: public GranuleCheck
{
 public:
  CheckMuid(const std::string &arm);
  virtual ~CheckMuid() {}

  int Init();

  int DcmFEMParityErrorCheck();
  int SubsystemCheck(Event *evt, const int iwhat);  // check the user words

 protected:

};

#endif /* __CHECKMUID_H__ */
