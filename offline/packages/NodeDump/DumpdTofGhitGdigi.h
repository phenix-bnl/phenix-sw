#ifndef __DUMPDTOFGHITGDIGI_H__
#define __DUMPDTOFGHITGDIGI_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofGhitGdigi : public DumpObject
{
 public:
  DumpdTofGhitGdigi(const std::string &NodeName);
  virtual ~DumpdTofGhitGdigi() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFGHITGDIGI_H__ */

