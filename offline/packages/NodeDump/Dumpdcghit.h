#ifndef __DUMPDCGHIT_H__
#define __DUMPDCGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpdcghit : public DumpObject
{
 public:
  Dumpdcghit(const std::string &NodeName);
  virtual ~Dumpdcghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDCGHIT_H__ */

