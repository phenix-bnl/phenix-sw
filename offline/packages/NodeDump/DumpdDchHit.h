#ifndef __DUMPDDCHHIT_H__
#define __DUMPDDCHHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdDchHit : public DumpObject
{
 public:
  DumpdDchHit(const std::string &NodeName);
  virtual ~DumpdDchHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDDCHHIT_H__ */

