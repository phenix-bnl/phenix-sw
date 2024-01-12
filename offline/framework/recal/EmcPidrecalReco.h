#ifndef __EMCPIDRECALRECO_H__
#define __EMCPIDRECALRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class TH2;

class EmcPidrecalReco : public Recalibrator
{
 public:
  EmcPidrecalReco(const std::string &name="EmcPidrecalReco");
  virtual ~EmcPidrecalReco() {}

  //  Standard methods
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  

 protected:

  float IsPion(const float m2emc, const float mom, const short charge);
  float IsKaon(const float m2emc, const float mom, const short charge);
  float IsProton(const float m2emc, const float mom, const short charge);
  TH2 *momtof;
  TH2 *momtofP;
  TH2 *momtofK;
  TH2 *momtofPi;
};

#endif /* __PIDRECALRECO_H__ */
