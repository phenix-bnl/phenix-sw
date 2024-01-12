#ifndef __DUMPTOFGHIT_H__
#define __DUMPTOFGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumptofghit : public DumpObject
{
 public:
  Dumptofghit(const std::string &NodeName);
  virtual ~Dumptofghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTOFGHIT_H__ */

