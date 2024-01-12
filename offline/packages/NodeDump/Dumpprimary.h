#ifndef __DUMPPRIMARY_H__
#define __DUMPPRIMARY_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpprimary : public DumpObject
{
 public:
  Dumpprimary(const std::string &NodeName);
  virtual ~Dumpprimary() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPRIMARY_H__ */

