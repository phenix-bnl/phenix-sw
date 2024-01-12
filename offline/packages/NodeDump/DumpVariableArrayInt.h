#ifndef __DUMPVARIABLEARRAYINT_H__
#define __DUMPVARIABLEARRAYINT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpVariableArrayInt : public DumpObject
{
 public:
  DumpVariableArrayInt(const std::string &NodeName);
  virtual ~DumpVariableArrayInt() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPVARIABLEARRAYINT_H__ */

