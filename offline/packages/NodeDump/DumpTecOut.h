#ifndef __DUMPTECOUT_H__
#define __DUMPTECOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTecOut : public DumpObject
{
 public:
  DumpTecOut(const std::string &NodeName);
  virtual ~DumpTecOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTECOUT_H__ */

