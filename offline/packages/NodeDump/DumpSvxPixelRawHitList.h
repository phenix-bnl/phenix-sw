#ifndef DUMPSVXPIXELRAWHITLIST_H
#define DUMPSVXPIXELRAWHITLIST_H

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxPixelRawHitList : public DumpObject
{
 public:
  DumpSvxPixelRawHitList(const std::string &NodeName);
  virtual ~DumpSvxPixelRawHitList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* DUMPSVXPIXELRAWHITLIST_H */

