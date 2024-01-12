#ifndef __DUMPSVXSEGMENTLIST_H__
#define __DUMPSVXSEGMENTLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxSegmentList : public DumpObject
{
 public:
  DumpSvxSegmentList(const std::string &NodeName);
  virtual ~DumpSvxSegmentList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSVXSEGMENTLIST_H__ */

