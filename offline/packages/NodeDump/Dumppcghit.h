#ifndef __DUMPPCGHIT_H__
#define __DUMPPCGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumppcghit : public DumpObject
{
 public:
  Dumppcghit(const std::string &NodeName);
  virtual ~Dumppcghit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPCGHIT_H__ */

