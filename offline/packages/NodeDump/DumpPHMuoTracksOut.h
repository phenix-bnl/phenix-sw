#ifndef __DUMPPHMUOTRACKSOUT_H__
#define __DUMPPHMUOTRACKSOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHMuoTracksOut : public DumpObject
{
 public:
  DumpPHMuoTracksOut(const std::string &NodeName);
  virtual ~DumpPHMuoTracksOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHMUOTRACKSOUT_H__ */

