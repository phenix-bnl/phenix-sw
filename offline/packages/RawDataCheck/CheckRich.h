#ifndef CHECKRICH_H__
#define CHECKRICH_H__

#include "GranuleCheck.h"

class Event;

class CheckRich: public GranuleCheck
{
 public:
  CheckRich(const std::string &arm);
  virtual ~CheckRich() {}

  int Init();

  int DcmFEMParityErrorCheck();
  int FEMParityErrorCheck();

 protected:

};

#endif /* __CHECKRICH_H__ */
