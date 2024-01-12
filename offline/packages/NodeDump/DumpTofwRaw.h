#ifndef __DUMPTOFWRAW_H__
#define __DUMPTOFWRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpTofwRaw : public DumpObject
{
 public:
  DumpTofwRaw(const std::string &NodeName);
  virtual ~DumpTofwRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPTOFWRAW_H__ */

