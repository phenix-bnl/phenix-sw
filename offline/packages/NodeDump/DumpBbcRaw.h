#ifndef __DUMPBBCRAW_H__
#define __DUMPBBCRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpBbcRaw : public DumpObject
{
 public:
  DumpBbcRaw(const std::string &NodeName);
  virtual ~DumpBbcRaw() {}

 protected:
  int process_Node(PHNode *myNode);
};

#endif /* __DUMPBBCRAW_H__ */

