#ifndef __DUMPHBDCELLLIST_H__
#define __DUMPHBDCELLLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpHbdCellList : public DumpObject
{
 public:
  DumpHbdCellList(const std::string &NodeName);
  virtual ~DumpHbdCellList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPHBDCELLLIST_H__ */

