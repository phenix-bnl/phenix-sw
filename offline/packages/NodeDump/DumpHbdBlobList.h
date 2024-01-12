#ifndef __DUMPHBDBLOBLIST_H__
#define __DUMPHBDBLOBLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpHbdBlobList : public DumpObject
{
 public:
  DumpHbdBlobList(const std::string &NodeName);
  virtual ~DumpHbdBlobList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPHBDBLOBLIST_H__ */

