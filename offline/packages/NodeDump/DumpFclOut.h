#ifndef __DUMPFCLOUT_H__
#define __DUMPFCLOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpFclOut : public DumpObject
{
 public:
  DumpFclOut(const std::string &NodeName);
  virtual ~DumpFclOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPFCLOUT_H__ */

