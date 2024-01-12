#ifndef __DUMPFKIN_H__
#define __DUMPFKIN_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpfkin : public DumpObject
{
 public:
  Dumpfkin(const std::string &NodeName);
  virtual ~Dumpfkin() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPFKIN_H__ */

