#ifndef __DUMPACCHIT_H__
#define __DUMPACCHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpAccHit : public DumpObject
{
 public:
  DumpAccHit(const std::string &NodeName);
  virtual ~DumpAccHit() {}

 protected:
  int process_Node(PHNode *myNode);
};

#endif /* __DUMPACCHIT_H__ */

