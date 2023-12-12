#ifndef __CHECKHBD_H__
#define __CHECKHBD_H__

#include <GranuleCheck.h>

class Event;

class CheckHbd: public GranuleCheck
{
 public:
  CheckHbd();
  virtual ~CheckHbd() {}

  int Init();
  int DcmFEMParityErrorCheck();
  int FEMEventSequenceCheck();
  unsigned int LocalEventCounter();

 protected:

};

#endif /* __CHECKHBD_H__ */
