#ifndef __DUMPDEMCGEATRACKCLUSTER_H__
#define __DUMPDEMCGEATRACKCLUSTER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaTrackCluster : public DumpObject
{
 public:
  DumpdEmcGeaTrackCluster(const std::string &NodeName);
  virtual ~DumpdEmcGeaTrackCluster() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEATRACKCLUSTER_H__ */

