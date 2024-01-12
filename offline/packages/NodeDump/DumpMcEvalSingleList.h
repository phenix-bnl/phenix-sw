#ifndef __DUMPMCEVALSINGLELIST_H__
#define __DUMPMCEVALSINGLELIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpMcEvalSingleList : public DumpObject
{
 public:
  DumpMcEvalSingleList(const std::string &NodeName);
  virtual ~DumpMcEvalSingleList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMCEVALSINGLELIST_H__ */

