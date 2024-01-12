#ifndef __DUMPHBDRAWLIST_H__
#define __DUMPHBDRAWLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpHbdRawList : public DumpObject
{
 public:
  DumpHbdRawList(const std::string &NodeName);
  virtual ~DumpHbdRawList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPHBDRAWLIST_H__ */

