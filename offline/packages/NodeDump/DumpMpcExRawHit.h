#ifndef __DUMPMPCEXRAWHIT_H__
#define __DUMPMPCEXRAWHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpMpcExRawHit : public DumpObject
{
 public:
  DumpMpcExRawHit(const std::string &NodeName);
  virtual ~DumpMpcExRawHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCEXRAWHIT_H__ */

