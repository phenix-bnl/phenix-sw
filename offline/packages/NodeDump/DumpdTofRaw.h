#ifndef __DUMPDTOFRAW_H__
#define __DUMPDTOFRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofRaw : public DumpObject
{
 public:
  DumpdTofRaw(const std::string &NodeName);
  virtual ~DumpdTofRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFRAW_H__ */

