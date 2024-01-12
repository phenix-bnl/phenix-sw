#ifndef __DUMPSPINDATAEVENTOUT_H__
#define __DUMPSPINDATAEVENTOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSpinDataEventOut : public DumpObject
{
 public:
  DumpSpinDataEventOut(const std::string &NodeName);
  virtual ~DumpSpinDataEventOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSPINDATAEVENTOUT_H__ */

