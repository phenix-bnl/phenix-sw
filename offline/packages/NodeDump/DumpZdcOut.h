#ifndef __DUMPZDCOUT_H__
#define __DUMPZDCOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpZdcOut : public DumpObject
{
 public:
  DumpZdcOut(const std::string &NodeName);
  virtual ~DumpZdcOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPZDCOUT_H__ */

