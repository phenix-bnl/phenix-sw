#ifndef __DUMPDTECGHITRAW_H__
#define __DUMPDTECGHITRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTecGhitRaw : public DumpObject
{
 public:
  DumpdTecGhitRaw(const std::string &NodeName);
  virtual ~DumpdTecGhitRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTECGHITRAW_H__ */

