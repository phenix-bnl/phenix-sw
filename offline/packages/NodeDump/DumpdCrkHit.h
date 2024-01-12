#ifndef __DUMPDCRKHIT_H__
#define __DUMPDCRKHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdCrkHit : public DumpObject
{
 public:
  DumpdCrkHit(const std::string &NodeName);
  virtual ~DumpdCrkHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDCRKHIT_H__ */

