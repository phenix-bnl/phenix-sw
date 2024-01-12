#ifndef __DUMPHEADER_H__
#define __DUMPHEADER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class Dumpheader : public DumpObject
{
 public:
  Dumpheader(const std::string &NodeName);
  virtual ~Dumpheader() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPHEADER_H__ */

