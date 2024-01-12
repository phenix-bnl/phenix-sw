#ifndef __Matchrecal_dAu_RECO_H__
#define __Matchrecal_dAu_RECO_H__

#include <string>
#include "Recalibrator.h"

class PHCentralTrack;
class PHCompositeNode;

//
//  This recalibrator does the PC3 matches (only)
//  for the Run-3 dAu data.  It was too complicated to 
//  merge with the Run4 code so we made a separate module.
//
//                   TKH ZC  1-22-2006
//

class Matchrecal_dAu_Reco : public Recalibrator
{
 public:
  Matchrecal_dAu_Reco(const char*name="Matchrecal_dAu_Reco");
  virtual ~Matchrecal_dAu_Reco() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

 protected:
  int runNumber;
  PHCentralTrack *d_cnt;
  std::string CentralTrackType;
  int run_Burn_pc3_match;

};

#endif /* __TOFRECALRECO_H__ */
