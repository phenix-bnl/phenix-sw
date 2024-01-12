#ifndef __DUMPFCLRAW_H__
#define __DUMPFCLRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpFclRaw : public DumpObject
{
 public:
  DumpFclRaw(const std::string &NodeName);
  virtual ~DumpFclRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPFCLRAW_H__ */

