#ifndef __DUMPERTOUT_H__
#define __DUMPERTOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpErtOut : public DumpObject
{
 public:
  DumpErtOut(const std::string &NodeName);
  virtual ~DumpErtOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPERTOUT_H__ */

