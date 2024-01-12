#ifndef __DUMPDETECTORGEOMETRY_H__
#define __DUMPDETECTORGEOMETRY_H__

#include "DumpObject.h"
#include <string>

class PHNode;

class DumpDetectorGeometry : public DumpObject
{
 public:
  DumpDetectorGeometry(const std::string &NodeName);
  virtual ~DumpDetectorGeometry() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPDETECTORGEOMETRY_H__ */

