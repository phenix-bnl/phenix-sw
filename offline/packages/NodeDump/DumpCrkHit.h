#ifndef __DUMPCRKHIT_H__
#define __DUMPCRKHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpCrkHit : public DumpObject
{
 public:
  DumpCrkHit(const std::string &NodeName);
  virtual ~DumpCrkHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPCRKHIT_H__ */

