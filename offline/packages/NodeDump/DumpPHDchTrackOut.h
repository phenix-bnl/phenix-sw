#ifndef __DUMPPHDCHTRACKOUT_H__
#define __DUMPPHDCHTRACKOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHDchTrackOut : public DumpObject
{
 public:
  DumpPHDchTrackOut(const std::string &NodeName);
  virtual ~DumpPHDchTrackOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHDCHTRACKOUT_H__ */

