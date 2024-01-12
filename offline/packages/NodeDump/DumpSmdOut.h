#ifndef __DUMPSMDOUT_H__
#define __DUMPSMDOUT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSmdOut : public DumpObject
{
 public:
  DumpSmdOut(const std::string &NodeName);
  virtual ~DumpSmdOut() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPSMDOUT_H__ */

