#ifndef __DUMPTRIGRUNLVL2_H__
#define __DUMPTRIGRUNLVL2_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTrigRunLvl2 : public DumpObject
{
 public:
  DumpTrigRunLvl2(const std::string &NodeName);
  virtual ~DumpTrigRunLvl2() {}

 protected:
   int process_Node(PHNode *mynode);
   int node_written;
};

#endif /* __DUMPTRIGRUNLVL2_H__ */

