#ifndef __DUMPDCRKREL2S_H__
#define __DUMPDCRKREL2S_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdCrkRel2s : public DumpObject
{
 public:
  DumpdCrkRel2s(const std::string &NodeName);
  virtual ~DumpdCrkRel2s() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDCRKREL2S_H__ */

