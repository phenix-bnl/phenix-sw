#ifndef __DUMPCRKRING_H__
#define __DUMPCRKRING_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpCrkRing : public DumpObject
{
 public:
  DumpCrkRing(const std::string &NodeName);
  virtual ~DumpCrkRing() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPCRKRING_H__ */

