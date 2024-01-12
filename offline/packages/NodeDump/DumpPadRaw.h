#ifndef __DUMPPADRAW_H__
#define __DUMPPADRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPadRaw : public DumpObject
{
 public:
  DumpPadRaw(const std::string &NodeName);
  virtual ~DumpPadRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPADRAW_H__ */

