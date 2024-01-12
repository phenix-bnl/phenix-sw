#ifndef __DUMPTECCLUSTERCONTAINER_H__
#define __DUMPTECCLUSTERCONTAINER_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpTecClusterContainer : public DumpObject
{
 public:
  DumpTecClusterContainer(const std::string &NodeName);
  virtual ~DumpTecClusterContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTECCLUSTERCONTAINER_H__ */

