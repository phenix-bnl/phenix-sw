#ifndef __DUMPDEMCCALIBTOWER_H__
#define __DUMPDEMCCALIBTOWER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcCalibTower : public DumpObject
{
 public:
  DumpdEmcCalibTower(const std::string &NodeName);
  virtual ~DumpdEmcCalibTower() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCCALIBTOWER_H__ */

