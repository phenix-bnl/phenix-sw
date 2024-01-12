#ifndef __DUMPDCHHITLINETABLE_H__
#define __DUMPDCHHITLINETABLE_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpDchHitLineTable : public DumpObject
{
 public:
  DumpDchHitLineTable(const std::string &NodeName);
  virtual ~DumpDchHitLineTable() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDCHHITLINETABLE_H__ */

