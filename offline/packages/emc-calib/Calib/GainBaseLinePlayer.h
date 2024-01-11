#ifndef __GAINBASELINEPLAYER_H__
#define __GAINBASELINEPLAYER_H__

#include "SubsysReco.h"
#include <string>
#include <vector>

class emcCalibrationDataHelper;

/** Test SubsysReco to do/undo gain base line removal. 
@ingroup calibration
*/

class GainBaseLinePlayer : public SubsysReco
{
 public:
  GainBaseLinePlayer(const char* runningmode);
  virtual ~GainBaseLinePlayer();

  int End(PHCompositeNode*);
  int EndRun(PHCompositeNode*);

  int Init(PHCompositeNode*);
  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

  int Reset(PHCompositeNode*);
  int ResetEvent(PHCompositeNode*);

  void Print(const char* what="ALL") const;

 private:
  emcCalibrationDataHelper* fCH;
  std::string fRunningMode;
  std::vector<float> fCorrectionFactor;
};

#endif
