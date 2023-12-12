#ifndef CHECKPAD_H__
#define CHECKPAD_H__

#include "GranuleCheck.h"

class Event;

class CheckPad: public GranuleCheck
{
 public:
  CheckPad(const std::string &arm);
  virtual ~CheckPad() {}

  int Init();

  int DcmFEMParityErrorCheck();
  int BadBeamClock();
// just to keep this method in case pkt 4008 goes bad again
// I do not have to reinvent this method
  int FEMClockCounterCheck4008(); 

 protected:

};

#endif /* __CHECKPAD_H__ */
