#ifndef __DUMPDDCHNIBBLEGHIT_H__
#define __DUMPDDCHNIBBLEGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdDchNibbleGhit : public DumpObject
{
 public:
  DumpdDchNibbleGhit(const std::string &NodeName);
  virtual ~DumpdDchNibbleGhit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDDCHNIBBLEGHIT_H__ */

