#ifndef CHECKFCAL_H__
#define CHECKFCAL_H__

#include "GranuleCheck.h"

class Event;

class CheckFcal: public GranuleCheck
{
 public:
  CheckFcal(const std::string &what="FCAL");
  virtual ~CheckFcal() {}

  int Init();

  int DcmFEMParityErrorCheck();

 protected:

};

#endif /* __CHECKFCAL_H__ */
