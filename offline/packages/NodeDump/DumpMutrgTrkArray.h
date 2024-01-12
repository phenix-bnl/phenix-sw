#ifndef __DUMPMUTRGTRKARRAY_H__
#define __DUMPMUTRGTRKARRAY_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpMutrgTrkArray : public DumpObject
{
 public:
  DumpMutrgTrkArray(const std::string &NodeName);
  virtual ~DumpMutrgTrkArray() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMUTRGTRKARRAY_H__ */

