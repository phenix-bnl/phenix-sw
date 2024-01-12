#ifndef __DUMPLVL2OUTARRAY_H__
#define __DUMPLVL2OUTARRAY_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpLvl2OutArray : public DumpObject
{
 public:
  DumpLvl2OutArray(const std::string &NodeName);
  virtual ~DumpLvl2OutArray() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPLVL2OUTARRAY_H__ */

