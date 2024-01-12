#ifndef __DUMPRPSUMXYOBJECT_H__
#define __DUMPRPSUMXYOBJECT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpRpSumXYObject : public DumpObject
{
 public:
  DumpRpSumXYObject(const std::string &NodeName);
  virtual ~DumpRpSumXYObject() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPRPSUMXYOBJECT_H__ */

