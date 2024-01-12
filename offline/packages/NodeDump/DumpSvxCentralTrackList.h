#ifndef DUMPSVXCENTRALTRACKLIST_H__
#define DUMPSVXCENTRALTRACKLIST_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpSvxCentralTrackList : public DumpObject
{
 public:
  DumpSvxCentralTrackList(const std::string &NodeName);
  virtual ~DumpSvxCentralTrackList() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* DUMPSVXCENTRALTRACKLIST_H__ */

