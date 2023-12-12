#ifndef CHECKACC_H__
#define CHECKACC_H__

#include "GranuleCheck.h"

class Event;

class CheckAcc: public GranuleCheck
{
 public:
  CheckAcc();
  virtual ~CheckAcc() {}

  int Init();
  int DcmFEMParityErrorCheck();
  int FEMParityErrorCheck();

 protected:

};

#endif /* __CHECKACC_H__ */
