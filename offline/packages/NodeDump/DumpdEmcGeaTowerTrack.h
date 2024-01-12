#ifndef __DUMPDEMCGEATOWERTRACK_H__
#define __DUMPDEMCGEATOWERTRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaTowerTrack : public DumpObject
{
 public:
  DumpdEmcGeaTowerTrack(const std::string &NodeName);
  virtual ~DumpdEmcGeaTowerTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEATOWERTRACK_H__ */

