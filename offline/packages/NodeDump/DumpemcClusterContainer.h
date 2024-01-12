#ifndef __DUMPEMCCLUSTERCONTAINER_H__
#define __DUMPEMCCLUSTERCONTAINER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpemcClusterContainer : public DumpObject
{
 public:
  DumpemcClusterContainer(const std::string &NodeName);
  virtual ~DumpemcClusterContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPEMCCLUSTERCONTAINER_H__ */

