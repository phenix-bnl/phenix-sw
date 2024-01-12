#ifndef __DUMPMPCRAWCONTAINER_H__
#define __DUMPMPCRAWCONTAINER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpmpcRawContainer : public DumpObject
{
 public:
  DumpmpcRawContainer(const std::string &NodeName);
  virtual ~DumpmpcRawContainer() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCRAWCONTAINER_H__ */

