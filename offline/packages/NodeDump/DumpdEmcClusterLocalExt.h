#ifndef __DUMPDEMCCLUSTERLOCALEXT_H__
#define __DUMPDEMCCLUSTERLOCALEXT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcClusterLocalExt : public DumpObject
{
 public:
  DumpdEmcClusterLocalExt(const std::string &NodeName);
  virtual ~DumpdEmcClusterLocalExt() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCCLUSTERLOCALEXT_H__ */

