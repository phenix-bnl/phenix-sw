#ifndef __FCLEVENT_H__
#define __FCLEVENT_H__

#include "FclCalib.h"

class FclIndexer;
class PHCompositeNode;
class PHTimeStamp;
class TFile;
class TNtuple;

class FclEvent
{
 public:

  FclEvent();                          
  virtual ~FclEvent();

  int event(PHCompositeNode *topNode); // the work-horse routine
  void saveDebug();

  int setCalibration(PHTimeStamp& time);
  int setCalibration(int runNumber);

 protected:

  TFile* ntupleFile;
  TNtuple* fcalNtuple;
  FclIndexer* indexer;
  FclCalib calibNorth;
  FclCalib calibSouth;
};

#endif
