#ifndef __DUMPTRIGLVL1_H__
#define __DUMPTRIGLVL1_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTrigLvl1 : public DumpObject
{
 public:
  DumpTrigLvl1(const std::string &NodeName);
  virtual ~DumpTrigLvl1() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTRIGLVL1_H__ */

