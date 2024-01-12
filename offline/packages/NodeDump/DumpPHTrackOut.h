#ifndef __DUMPPHTRACKOUT_H__
#define __DUMPPHTRACKOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHTrackOut : public DumpObject
{
 public:
  DumpPHTrackOut(const std::string &NodeName);
  virtual ~DumpPHTrackOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHTRACKOUT_H__ */

