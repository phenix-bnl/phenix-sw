#ifndef __DUMPPHCENTRALTRACK_H__
#define __DUMPPHCENTRALTRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHCentralTrack : public DumpObject
{
 public:
  DumpPHCentralTrack(const std::string &NodeName);
  virtual ~DumpPHCentralTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHCENTRALTRACK_H__ */

