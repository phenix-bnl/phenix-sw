#ifndef DUMPCRKPROJ_H__
#define DUMPCRKPROJ_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpCrkProj : public DumpObject
{
 public:
  DumpCrkProj(const std::string &NodeName);
  virtual ~DumpCrkProj() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* DUMPCRKPROJ_H__ */

