#ifndef __DUMPVTXOUT_H__
#define __DUMPVTXOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpVtxOut : public DumpObject
{
 public:
  DumpVtxOut(const std::string &NodeName);
  virtual ~DumpVtxOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPVTXOUT_H__ */

