#ifndef __DUMPHBDMINICELLLIST_H__
#define __DUMPHBDMINICELLLIST_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpHbdMiniCellList : public DumpObject
{
 public:
  DumpHbdMiniCellList(const std::string &NodeName);
  virtual ~DumpHbdMiniCellList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPHBDMINICELLLIST_H__ */

