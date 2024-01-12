#ifndef __DUMPTOFOUT_H__
#define __DUMPTOFOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTofOut : public DumpObject
{
 public:
  DumpTofOut(const std::string &NodeName);
  virtual ~DumpTofOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTOFOUT_H__ */

