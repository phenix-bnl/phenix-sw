#ifndef __DUMPBBCGHIT_H__
#define __DUMPBBCGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpbbcghit : public DumpObject
{
 public:
  Dumpbbcghit(const std::string &NodeName);
  virtual ~Dumpbbcghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPBBCGHIT_H__ */

