#ifndef __DUMPMPCTOWERCONTAINER_H__
#define __DUMPMPCTOWERCONTAINER_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpmpcTowerContainer : public DumpObject
{
 public:
  DumpmpcTowerContainer(const std::string &NodeName);
  virtual ~DumpmpcTowerContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCTOWERCONTAINER_H__ */

