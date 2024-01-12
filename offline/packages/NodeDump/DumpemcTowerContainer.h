#ifndef __DUMPEMCTOWERCONTAINER_H__
#define __DUMPEMCTOWERCONTAINER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpemcTowerContainer : public DumpObject
{
 public:
  DumpemcTowerContainer(const std::string &NodeName);
  virtual ~DumpemcTowerContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPEMCTOWERCONTAINER_H__ */

