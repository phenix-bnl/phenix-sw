#ifndef __DUMPKALFITOUT_H__
#define __DUMPKALFITOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpKalFitOut : public DumpObject
{
 public:
  DumpKalFitOut(const std::string &NodeName);
  virtual ~DumpKalFitOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPKALFITOUT_H__ */

