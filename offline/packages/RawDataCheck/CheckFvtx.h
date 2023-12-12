#ifndef CHECKFVTX_H__
#define CHECKFVTX_H__

#include "GranuleCheck.h"
#include <string>

class Event;

class CheckFvtx: public GranuleCheck
{
 public:
  CheckFvtx(const std::string &arm = "FVTX");
  virtual ~CheckFvtx() {}
  int Init();
  int DcmFEMParityErrorCheck(); // Compare the dcm2-calculated packet parity to the parity the FEM thinks it sent out
  void SetBeamClock(); 
  int GlinkCheck();
  unsigned int LocalEventCounter();
  int FEMClockCounterCheck();
  int ResetEvent();

 protected:
  int fvtx_standard_clock[4];

};

#endif /* __CHECKFVTX_H__ */
