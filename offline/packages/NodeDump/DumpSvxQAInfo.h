#ifndef __DUMPSVXQAINFO_H__
#define __DUMPSVXQAINFO_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxQAInfo : public DumpObject
{
 public:
  DumpSvxQAInfo(const std::string &NodeName);
  virtual ~DumpSvxQAInfo() {}

 protected:
   int process_Node(PHNode *mynode);
   int node_written;
};

#endif /* __DUMPSVXQAINFO_H__ */

