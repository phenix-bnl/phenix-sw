#ifndef __DUMPPADCLUSTER_H__
#define __DUMPPADCLUSTER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPadCluster : public DumpObject
{
 public:
  DumpPadCluster(const std::string &NodeName);
  virtual ~DumpPadCluster() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPADCLUSTER_H__ */

