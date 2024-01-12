#ifndef __DUMPDDCHTRACKSEXT_H__
#define __DUMPDDCHTRACKSEXT_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdDchTracksExt : public DumpObject
{
 public:
  DumpdDchTracksExt(const std::string &NodeName);
  virtual ~DumpdDchTracksExt() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDDCHTRACKSEXT_H__ */

