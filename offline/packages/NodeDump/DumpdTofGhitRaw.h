#ifndef __DUMPDTOFGHITRAW_H__
#define __DUMPDTOFGHITRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofGhitRaw : public DumpObject
{
 public:
  DumpdTofGhitRaw(const std::string &NodeName);
  virtual ~DumpdTofGhitRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFGHITRAW_H__ */

