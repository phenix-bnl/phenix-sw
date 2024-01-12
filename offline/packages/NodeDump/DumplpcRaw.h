#ifndef __DUMPLPCRAW_H__
#define __DUMPLPCRAW_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumplpcRaw : public DumpObject
{
 public:
  DumplpcRaw(const std::string &NodeName);
  virtual ~DumplpcRaw() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPLPCRAW_H__ */

