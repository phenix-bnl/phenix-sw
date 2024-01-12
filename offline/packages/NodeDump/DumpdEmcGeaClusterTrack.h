#ifndef __DUMPDEMCGEACLUSTERTRACK_H__
#define __DUMPDEMCGEACLUSTERTRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaClusterTrack : public DumpObject
{
 public:
  DumpdEmcGeaClusterTrack(const std::string &NodeName);
  virtual ~DumpdEmcGeaClusterTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEACLUSTERTRACK_H__ */

