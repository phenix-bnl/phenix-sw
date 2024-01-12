#ifndef __DUMPSVXRAWHITLIST_H__
#define __DUMPSVXRAWHITLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxRawhitList : public DumpObject
{
 public:
  DumpSvxRawhitList(const std::string &NodeName);
  virtual ~DumpSvxRawhitList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSVXRAWHITLIST_H__ */

