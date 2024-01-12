#ifndef DUMPEMCCLUSTERAUXINFOCONTAINER_H__
#define DUMPEMCCLUSTERAUXINFOCONTAINER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpemcClusterAuxInfoContainer : public DumpObject
{
 public:
  DumpemcClusterAuxInfoContainer(const std::string &NodeName);
  virtual ~DumpemcClusterAuxInfoContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* DUMPEMCCLUSTERAUXINFOCONTAINER_H__ */

