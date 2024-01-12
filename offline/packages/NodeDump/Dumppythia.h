#ifndef __DUMPPYTHIA_H__
#define __DUMPPYTHIA_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumppythia : public DumpObject
{
 public:
  Dumppythia(const std::string &NodeName);
  virtual ~Dumppythia() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPYTHIA_H__ */

