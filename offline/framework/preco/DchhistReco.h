#ifndef __DCHHISTRECO_H__
#define __DCHHISTRECO_H__

#include "SubsysReco.h"

class TFile;
class TNtuple;

class DchhistReco: public SubsysReco
{
 public:
  DchhistReco(const char *name = "DCHHIST");
  virtual ~DchhistReco() {}

  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 protected:
  TFile *Datafile;

  bool doStereoWires;
  bool doStereoZed;
  bool doUValignment;

  void fillStereoWires(PHCompositeNode *topNode);
  void fillStereoZed  (PHCompositeNode *topNode);
  void fillUValignment(PHCompositeNode *topNode);

  TNtuple *stereoWires;
  TNtuple *stereoZed  ;
  TNtuple *UValignment;
};

#endif /* __DCHHISTRECO_H__ */
