#ifndef __DUMPTECGHIT_H__
#define __DUMPTECGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumptecghit : public DumpObject
{
 public:
  Dumptecghit(const std::string &NodeName);
  virtual ~Dumptecghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTECGHIT_H__ */

