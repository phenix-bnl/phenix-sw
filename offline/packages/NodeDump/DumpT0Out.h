#ifndef __DUMPT0OUT_H__
#define __DUMPT0OUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpT0Out : public DumpObject
{
 public:
  DumpT0Out(const std::string &NodeName);
  virtual ~DumpT0Out() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPT0OUT_H__ */

