#ifndef __DUMPZDCRAW_H__
#define __DUMPZDCRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpZdcRaw : public DumpObject
{
 public:
  DumpZdcRaw(const std::string &NodeName);
  virtual ~DumpZdcRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPZDCRAW_H__ */

