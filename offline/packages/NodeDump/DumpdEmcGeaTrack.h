#ifndef __DUMPDEMCGEATRACK_H__
#define __DUMPDEMCGEATRACK_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpdEmcGeaTrack : public DumpObject
{
 public:
  DumpdEmcGeaTrack(const std::string &NodeName);
  virtual ~DumpdEmcGeaTrack() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDEMCGEATRACK_H__ */

