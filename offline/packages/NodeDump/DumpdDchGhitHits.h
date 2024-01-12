#ifndef __DUMPDDCHGHITHITS_H__
#define __DUMPDDCHGHITHITS_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdDchGhitHits : public DumpObject
{
 public:
  DumpdDchGhitHits(const std::string &NodeName);
  virtual ~DumpdDchGhitHits() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDDCHGHITHITS_H__ */

