#ifndef __DUMPSVXEVENTINFO_H__
#define __DUMPSVXEVENTINFO_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxEventInfo : public DumpObject
{
 public:
  DumpSvxEventInfo(const std::string &NodeName);
  virtual ~DumpSvxEventInfo() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSVXEVENTINFO_H__ */

