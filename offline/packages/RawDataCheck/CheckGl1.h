#ifndef CHECKGL1_H__
#define CHECKGL1_H__

#include "GranuleCheck.h"

class Event;

class CheckGl1: public GranuleCheck
{
 public:
  CheckGl1(const std::string &name = "GL1");
  virtual ~CheckGl1() {}

  int Init();
  int process_event(Event *e);
  int SubsystemCheck(Event *evt, const int iwhat);  // check for missing packet
  int FEMParityErrorCheck();

 protected:

};

#endif /* __CHECKGL1_H__ */
