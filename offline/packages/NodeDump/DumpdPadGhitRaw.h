#ifndef __DUMPDPADGHITRAW_H__
#define __DUMPDPADGHITRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdPadGhitRaw : public DumpObject
{
 public:
  DumpdPadGhitRaw(const std::string &NodeName);
  virtual ~DumpdPadGhitRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDPADGHITRAW_H__ */

