#ifndef __DUMPDTOFGDIGI_H__
#define __DUMPDTOFGDIGI_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofGdigi : public DumpObject
{
 public:
  DumpdTofGdigi(const std::string &NodeName);
  virtual ~DumpdTofGdigi() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFGDIGI_H__ */

