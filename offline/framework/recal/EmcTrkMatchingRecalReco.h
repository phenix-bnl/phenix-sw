#ifndef __EMCTRKMATCHINGRECALRECO_H__
#define __EMCTRKMATCHINGRECALRECO_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;


class EmcTrkMatchingRecalReco : public Recalibrator
{
 public:
  EmcTrkMatchingRecalReco(const std::string &name = "EmcTrkMatchingRecalReco");
  virtual ~EmcTrkMatchingRecalReco(){}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  int isValidRun(const int runno) const;

 private:
  float Emcsdz_e_Match(const float emcsdz_e, const float emcdz, const float alpha, const float mom, const float theta, const int emcsector);
  float Emcsdphi_e_Match(const float Px, const float Py, const float emcsdphi_e, const float emcdphi, const int charge, const float alpha, const float mom, const float z, const float phi, const int emcsector);
  int haverich;
};

#endif /*  __EMCTRKMATCHINGRECALRECO_H__ */

