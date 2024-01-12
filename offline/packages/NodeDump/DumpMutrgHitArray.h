#ifndef __DUMPMUTRGHITARRAY_H__
#define __DUMPMUTRGHITARRAY_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpMutrgHitArray : public DumpObject
{
 public:
  DumpMutrgHitArray(const std::string &NodeName);
  virtual ~DumpMutrgHitArray() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMUTRGHITARRAY_H__ */

