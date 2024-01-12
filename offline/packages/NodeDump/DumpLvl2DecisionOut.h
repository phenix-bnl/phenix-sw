#ifndef __DUMPLVL2DECISIONOUT_H__
#define __DUMPLVL2DECISIONOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpLvl2DecisionOut : public DumpObject
{
 public:
  DumpLvl2DecisionOut(const std::string &NodeName);
  virtual ~DumpLvl2DecisionOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPLVL2DECISIONOUT_H__ */

