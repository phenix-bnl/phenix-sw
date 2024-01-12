#ifndef __DUMPMPCCLUSTERCONTAINER_H__
#define __DUMPMPCCLUSTERCONTAINER_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpmpcClusterContainer : public DumpObject
{
 public:
  DumpmpcClusterContainer(const std::string &NodeName);
  virtual ~DumpmpcClusterContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCCLUSTERCONTAINER_H__ */

