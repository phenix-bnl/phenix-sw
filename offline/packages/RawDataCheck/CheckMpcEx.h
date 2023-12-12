#ifndef CHECKMPCEX_H__
#define CHECKMPCEX_H__

#include "GranuleCheck.h"

class CheckMpcEx: public GranuleCheck
{
 public:
  CheckMpcEx(const std::string &system);
  virtual ~CheckMpcEx() {}

  int Init();

  void SetBeamClock();
  unsigned int LocalEventCounter();
  int DcmFEMParityErrorCheck(); // Dcm does not recalculte fem parity

 protected:
  CheckMpcEx() {}
};

#endif /* __CHECKMPCEX_H__ */
