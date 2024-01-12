#ifndef __DUMPPHMUOTRACKSADC_H__
#define __DUMPPHMUOTRACKSADC_H__

#include "DumpObject.h"

#include <string>

class PHNode;

class DumpPHMuoTracksAdc : public DumpObject
{
 public:
  DumpPHMuoTracksAdc(const std::string &NodeName);
  virtual ~DumpPHMuoTracksAdc() {}

 protected:
   int process_Node(PHNode *mynode);
};

#endif /* __DUMPPHMUOTRACKSADC_H__ */

