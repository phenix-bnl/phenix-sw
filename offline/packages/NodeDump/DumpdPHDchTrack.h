#ifndef __DUMPDPHDCHTRACK_H__
#define __DUMPDPHDCHTRACK_H__

#include "DumpObject.h"

class PHNode;

#include <string>

class DumpdPHDchTrack : public DumpObject
{
 public:
  DumpdPHDchTrack(const std::string &NodeName);
  virtual ~DumpdPHDchTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDPHDCHTRACK_H__ */

