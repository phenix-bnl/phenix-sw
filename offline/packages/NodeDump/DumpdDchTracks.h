#ifndef __DUMPDDCHTRACKS_H__
#define __DUMPDDCHTRACKS_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdDchTracks : public DumpObject
{
 public:
  DumpdDchTracks(const std::string &NodeName);
  virtual ~DumpdDchTracks() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDDCHTRACKS_H__ */

