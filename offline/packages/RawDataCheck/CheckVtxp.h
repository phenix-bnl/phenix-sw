#ifndef __CHECKVTXP_H__
#define __CHECKVTXP_H__

#include "GranuleCheck.h"

#include <map>
#include <string>

class Event;

class CheckVtxp: public GranuleCheck
{
 public:
  CheckVtxp(const std::string &arm);
  virtual ~CheckVtxp() {}
  int Init();
  int DcmFEMParityErrorCheck(); // Compare the dcm2-calculated packet parity to the parity the FEM thinks it sent out
  void SetBeamClock(); 
  int GlinkCheck();
  int LocalEventNumberOffset();
  int SubsystemCheck(Event *evt, const int iwhat);
  int EndRun(const int runno);

 protected:
  CheckVtxp() {}

  int savebeamclk03;
  int savebeamclk47;
  std::map<int, int> neventssubsyscheck;
  std::map<int, int> lockloss_a;
  std::map<int, int> lockloss_b;
  std::map<int, int> sizerr_a;
  std::map<int, int> sizerr_b;
  std::map<int, int> parityerr;
  std::map<int, int> map_beamclk03;
  std::map<int, int> map_beamclk47;
  std::map<int, int> spiroevtcnt;
};

#endif /* __CHECKVTXP_H__ */
