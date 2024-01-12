#ifndef __DUMPDBBCRAW_H__
#define __DUMPDBBCRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdBbcRaw : public DumpObject
{
 public:
  DumpdBbcRaw(const std::string &NodeName);
  virtual ~DumpdBbcRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDBBCRAW_H__ */

