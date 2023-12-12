#ifndef CHECKDCH_H__
#define CHECKDCH_H__

#include "GranuleCheck.h"

class Event;

class CheckDch: public GranuleCheck
{
 public:
  CheckDch(const std::string &arm);
  virtual ~CheckDch() {}

  int Init();
  int DcmFEMParityErrorCheck();

 protected:

};

#endif /* __CHECKDCH_H__ */
