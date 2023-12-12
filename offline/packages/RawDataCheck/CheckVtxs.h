#ifndef __CHECKVTXS_H__
#define __CHECKVTXS_H__

#include "GranuleCheck.h"

#include <map>
#include <string>

class Event;

class CheckVtxs: public GranuleCheck
{
 public:
  CheckVtxs(const std::string &arm);
  virtual ~CheckVtxs() {}
  int Init();
  int DcmFEMParityErrorCheck(); // Compare the dcm2-calculated packet parity to the parity the FEM thinks it sent out
  void SetBeamClock(); 
  int GlinkCheck();
  unsigned int LocalEventCounter();
  int SubsystemCheck(Event *evt, const int iwhat);
  int EndRun(const int runno);

 protected:
  CheckVtxs() {}
  std::map<int, int> neventssubsyscheck;
  std::map<int, int> flag_bit;
  std::map<int, int> rccbadbeamclk;
  std::map<int, int> cellidproblem;


};

#endif /* __CHECKVTXS_H__ */
