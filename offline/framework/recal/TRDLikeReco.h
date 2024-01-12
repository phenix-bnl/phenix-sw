#ifndef __TRDLIKERECO_H__
#define __TRDLIKERECO_H__

#include "Recalibrator.h"
#include "PHTimeStamp.h"

class TRDLike;

class TRDLikeReco : public Recalibrator
{
 public:

  TRDLikeReco();

  virtual ~TRDLikeReco();

  int Init(PHCompositeNode*);
  int InitRun(PHCompositeNode*);
  int process_event(PHCompositeNode*);
  int isValidRun(const int runno) const;

 private:
  TRDLike* LNH[48];
};

#endif
