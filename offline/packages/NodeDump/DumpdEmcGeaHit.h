#ifndef __DUMPDEMCGEAHIT_H__
#define __DUMPDEMCGEAHIT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaHit : public DumpObject
{
 public:
  DumpdEmcGeaHit(const std::string &NodeName);
  virtual ~DumpdEmcGeaHit() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEAHIT_H__ */

