#ifndef CHECKERT_H__
#define CHECKERT_H__

#include "GranuleCheck.h"

class Event;

class CheckErt: public GranuleCheck
{
 public:
  CheckErt(const std::string &arm);
  virtual ~CheckErt() {}

  int Init();
  int DcmFEMParityErrorCheck();

 protected:

};

#endif /* __CHECKERT_H__ */
