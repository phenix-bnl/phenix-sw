#ifndef __DUMPAERGEAHITS_H__
#define __DUMPAERGEAHITS_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpAerGeaHits : public DumpObject
{
 public:
  DumpAerGeaHits(const std::string &NodeName);
  virtual ~DumpAerGeaHits() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPAERGEAHITS_H__ */

