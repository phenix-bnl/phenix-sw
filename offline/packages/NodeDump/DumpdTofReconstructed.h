#ifndef __DUMPDTOFRECONSTRUCTED_H__
#define __DUMPDTOFRECONSTRUCTED_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdTofReconstructed : public DumpObject
{
 public:
  DumpdTofReconstructed(const std::string &NodeName);
  virtual ~DumpdTofReconstructed() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDTOFRECONSTRUCTED_H__ */

