#ifndef __CHECKRPC_H__
#define __CHECKRPC_H__

#include "GranuleCheck.h"

class CheckRpc: public GranuleCheck
{
 public:
  CheckRpc(const std::string &system);
  virtual ~CheckRpc() {}

  int Init();

  int DcmFEMParityErrorCheck(); // Dcm does not recalculate fem parity
  unsigned int LocalEventCounter();
  int FEMEventSequenceCheck();
  void SetBeamClock();
  int GlinkCheck();

 protected:
  CheckRpc() {}
};

#endif /* __CHECKRPC_H__ */
