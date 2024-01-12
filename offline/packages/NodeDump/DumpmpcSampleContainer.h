#ifndef __DUMPMPCSAMPLECONTAINER_H__
#define __DUMPMPCSAMPLECONTAINER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpmpcSampleContainer : public DumpObject
{
 public:
  DumpmpcSampleContainer(const std::string &NodeName);
  virtual ~DumpmpcSampleContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCSAMPLECONTAINER_H__ */

