#ifndef __DUMPTOFWHIT_H__
#define __DUMPTOFWHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTofwHit : public DumpObject
{
 public:
  DumpTofwHit(const std::string &NodeName);
  virtual ~DumpTofwHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTOFWHIT_H__ */

