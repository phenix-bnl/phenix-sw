#ifndef __DUMPDTOFRAWREC_H__
#define __DUMPDTOFRAWREC_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofRawRec : public DumpObject
{
 public:
  DumpdTofRawRec(const std::string &NodeName);
  virtual ~DumpdTofRawRec() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFRAWREC_H__ */

