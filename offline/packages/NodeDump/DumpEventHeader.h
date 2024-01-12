#ifndef __DUMPEVENTHEADER_H__
#define __DUMPEVENTHEADER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpEventHeader : public DumpObject
{
 public:
  DumpEventHeader(const std::string &NodeName);
  virtual ~DumpEventHeader() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPEVENTHEADER_H__ */

