#ifndef __DUMPPDBCALBANKSAVE_H__
#define __DUMPPDBCALBANKSAVE_H__

#include <DumpObject.h>

#include <string>

class PHNode;

class DumpPdbCalBankSave : public DumpObject
{
 public:
  DumpPdbCalBankSave(const std::string &NodeName);
  virtual ~DumpPdbCalBankSave() {}

 protected:
   int process_Node(PHNode *mynode);
   int node_written;
};

#endif /* __DUMPPDBCALBANKSAVE_H__ */

