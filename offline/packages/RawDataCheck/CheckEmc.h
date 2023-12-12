#ifndef CHECKEMC_H__
#define CHECKEMC_H__

#include "GranuleCheck.h"

class Event;

class CheckEmc: public GranuleCheck
{
 public:
  CheckEmc(const std::string &arm = "EAST", const std::string &sector = "TOP");
  virtual ~CheckEmc() {}

  int Init();
  int DcmFEMParityErrorCheck();

 protected:

};

#endif /* __CHECKEMC_H__ */
