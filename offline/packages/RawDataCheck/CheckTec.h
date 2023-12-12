#ifndef __CHECKTEC_H__
#define __CHECKTEC_H__

#include "GranuleCheck.h"


class Event;

class CheckTec: public GranuleCheck
{
 public:
  CheckTec();
  virtual ~CheckTec() {}

  int Init();

  unsigned int LocalEventCounter(); // Local Event Counter needs + 1 since it starts at zero and rollover handling
  int DcmFEMParityErrorCheck(); // Dcm does not recalculte fem parity

 protected:

};

#endif /* __CHECKTEC_H__ */
