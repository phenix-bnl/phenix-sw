#ifndef __DUMPPHGLOBAL_H__
#define __DUMPPHGLOBAL_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHGlobal : public DumpObject
{
 public:
  DumpPHGlobal(const std::string &NodeName);
  virtual ~DumpPHGlobal() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHGLOBAL_H__ */

