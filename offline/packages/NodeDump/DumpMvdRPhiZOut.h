#ifndef __DUMPMVDRPHIZOUT_H__
#define __DUMPMVDRPHIZOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpMvdRPhiZOut : public DumpObject
{
 public:
  DumpMvdRPhiZOut(const std::string &NodeName);
  virtual ~DumpMvdRPhiZOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPMVDRPHIZOUT_H__ */

