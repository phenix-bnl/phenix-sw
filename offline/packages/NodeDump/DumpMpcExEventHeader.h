#ifndef __DUMPMPCEXEVENTHEADER_H__
#define __DUMPMPCEXEVENTHEADER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpMpcExEventHeader : public DumpObject
{
 public:
  DumpMpcExEventHeader(const std::string &NodeName);
  virtual ~DumpMpcExEventHeader() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMPCEXEVENTHEADER_H__ */

