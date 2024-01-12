#ifndef __DUMPBBCOUT_H__
#define __DUMPBBCOUT_H__


#include "DumpObject.h"

#include <string>

class PHNode;

class DumpBbcOut : public DumpObject
{
 public:
  DumpBbcOut(const std::string &NodeName);
  virtual ~DumpBbcOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPBBCOUT_H__ */

