#ifndef __DUMPPREVIOUSEVENT_H__
#define __DUMPPREVIOUSEVENT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPreviousEvent : public DumpObject
{
 public:
  DumpPreviousEvent(const std::string &NodeName);
  virtual ~DumpPreviousEvent() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPREVIOUSEVENT_H__ */

