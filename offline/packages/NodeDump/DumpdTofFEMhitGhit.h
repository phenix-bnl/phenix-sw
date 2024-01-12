#ifndef __DUMPDTOFFEMHITGHIT_H__
#define __DUMPDTOFFEMHITGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofFEMhitGhit : public DumpObject
{
 public:
  DumpdTofFEMhitGhit(const std::string &NodeName);
  virtual ~DumpdTofFEMhitGhit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFFEMHITGHIT_H__ */

