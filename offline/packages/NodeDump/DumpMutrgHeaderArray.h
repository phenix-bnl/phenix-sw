#ifndef __DUMPMUTRGHEADERARRAY_H__
#define __DUMPMUTRGHEADERARRAY_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpMutrgHeaderArray : public DumpObject
{
 public:
  DumpMutrgHeaderArray(const std::string &NodeName);
  virtual ~DumpMutrgHeaderArray() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMUTRGHEADERARRAY_H__ */

