#ifndef __DUMPDCHTRACK_H__
#define __DUMPDCHTRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpDchTrack : public DumpObject
{
 public:
  DumpDchTrack(const std::string &NodeName);
  virtual ~DumpDchTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDCHTRACK_H__ */

