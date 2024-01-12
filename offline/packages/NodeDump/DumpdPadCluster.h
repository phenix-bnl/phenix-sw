#ifndef __DUMPDPADCLUSTER_H__
#define __DUMPDPADCLUSTER_H__

#include "DumpObject.h"

class PHNode;

#include <string>

class DumpdPadCluster : public DumpObject
{
 public:
  DumpdPadCluster(const std::string &NodeName);
  virtual ~DumpdPadCluster() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDPADCLUSTER_H__ */

