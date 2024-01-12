#ifndef __DUMPDPADRAW_H__
#define __DUMPDPADRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdPadRaw : public DumpObject
{
 public:
  DumpdPadRaw(const std::string &NodeName);
  virtual ~DumpdPadRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDPADRAW_H__ */

