#ifndef __DUMPACCRAW_H__
#define __DUMPACCRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpAccRaw : public DumpObject
{
 public:
  DumpAccRaw(const std::string &NodeName);
  virtual ~DumpAccRaw() {}

 protected:
  int process_Node(PHNode *myNode);
};

#endif /* __DUMPACCRAW_H__ */

