#ifndef __DUMPDPADGHITCLUS_H__
#define __DUMPDPADGHITCLUS_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdPadGhitClus : public DumpObject
{
 public:
  DumpdPadGhitClus(const std::string &NodeName);
  virtual ~DumpdPadGhitClus() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDPADGHITCLUS_H__ */

