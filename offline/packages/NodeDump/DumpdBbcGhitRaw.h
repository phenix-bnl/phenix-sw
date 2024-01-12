#ifndef __DUMPDBBCGHITRAW_H__
#define __DUMPDBBCGHITRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdBbcGhitRaw : public DumpObject
{
 public:
  DumpdBbcGhitRaw(const std::string &NodeName);
  virtual ~DumpdBbcGhitRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDBBCGHITRAW_H__ */

