#ifndef __DUMPDTOFGDIGIREC_H__
#define __DUMPDTOFGDIGIREC_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofGdigiRec : public DumpObject
{
 public:
  DumpdTofGdigiRec(const std::string &NodeName);
  virtual ~DumpdTofGdigiRec() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFGDIGIREC_H__ */

