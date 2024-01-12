#ifndef __DUMPPHCENTRALTRACKV24_H__
#define __DUMPPHCENTRALTRACKV24_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHCentralTrackv24 : public DumpObject
{
 public:
  DumpPHCentralTrackv24(const std::string &NodeName);
  virtual ~DumpPHCentralTrackv24() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHCENTRALTRACKV24_H__ */

