#ifndef __DUMPCRKGHIT_H__
#define __DUMPCRKGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpcrkghit : public DumpObject
{
 public:
  Dumpcrkghit(const std::string &NodeName);
  virtual ~Dumpcrkghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPCRKGHIT_H__ */

