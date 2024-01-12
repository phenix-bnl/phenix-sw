#ifndef __DUMPACCCLUSTER_H__
#define __DUMPACCCLUSTER_H__

#include "DumpObject.h"
#include <string>

#include <string>

class PHNode;

class DumpAccCluster : public DumpObject
{
 public:
  DumpAccCluster(const std::string &NodeName);
  virtual ~DumpAccCluster() {}

 protected:
  int process_Node(PHNode *myNode);
};

#endif /* __DUMPACCCLUSTER_H__ */

