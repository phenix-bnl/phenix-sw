#ifndef __DUMPSVXCLUSTERLIST_H__
#define __DUMPSVXCLUSTERLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxClusterList : public DumpObject
{
 public:
  DumpSvxClusterList(const std::string &NodeName);
  virtual ~DumpSvxClusterList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSVXCLUSTERLIST_H__ */

