#ifndef __DUMPDPADNIBBLEGHIT_H__
#define __DUMPDPADNIBBLEGHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdPadNibbleGhit : public DumpObject
{
 public:
  DumpdPadNibbleGhit(const std::string &NodeName);
  virtual ~DumpdPadNibbleGhit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPADNIBBLEGHIT_H__ */

