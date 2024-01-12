#ifndef __DUMPCGLTRACK_H__
#define __DUMPCGLTRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpCglTrack : public DumpObject
{
 public:
  DumpCglTrack(const std::string &NodeName);
  virtual ~DumpCglTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPCGLTRACK_H__ */

