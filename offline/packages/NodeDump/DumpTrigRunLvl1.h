#ifndef __DUMPTRIGRUNLVL1_H__
#define __DUMPTRIGRUNLVL1_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpTrigRunLvl1 : public DumpObject
{
 public:
  DumpTrigRunLvl1(const std::string &NodeName);
  virtual ~DumpTrigRunLvl1() {}

 protected:
   int process_Node(PHNode *mynode);
   int node_written;
};

#endif /* __DUMPTRIGRUNLVL1_H__ */

