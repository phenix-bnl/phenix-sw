#ifndef __DUMPSVXRAWHITCLUSTERLIST_H__
#define __DUMPSVXRAWHITCLUSTERLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxRawhitClusterList : public DumpObject
{
 public:
  DumpSvxRawhitClusterList(const std::string &NodeName);
  virtual ~DumpSvxRawhitClusterList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSVXRAWHITCLUSTERLIST_H__ */

