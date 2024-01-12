#ifndef __DUMPFLAGS_H__
#define __DUMPFLAGS_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpFlags : public DumpObject
{
 public:
  DumpFlags(const std::string &NodeName);
  virtual ~DumpFlags() {}

 protected:
   int process_Node(PHNode *mynode);
   int node_written;
};

#endif /* __DUMPFLAGS_H__ */

