#ifndef __BBCRECAL_H__
#define __BBCRECAL_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Bbcrecal : public Recalibrator
{
 public:

  Bbcrecal();
  virtual ~Bbcrecal() {}

  int  process_event(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
};

#endif /* __BBCRECAL_H__ */
