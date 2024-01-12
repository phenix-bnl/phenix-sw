#ifndef __DUMPDEMCGEATRACKTOWER_H__
#define __DUMPDEMCGEATRACKTOWER_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaTrackTower : public DumpObject
{
 public:
  DumpdEmcGeaTrackTower(const std::string &NodeName);
  virtual ~DumpdEmcGeaTrackTower() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEATRACKTOWER_H__ */

