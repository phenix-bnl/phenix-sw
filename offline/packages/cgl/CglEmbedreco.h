#ifndef CGLEMBEDRECO_H__
#define CGLEMBEDRECO_H__

#include "SubsysReco.h"

class cglDetectorGeo;
class cglHitAssociate;
class mPHDchTrackModel;
class mPHLineTrack;
class PHCompositeNode;
class TriggerHelper;

class CglEmbedreco: public SubsysReco
{
 public:
  CglEmbedreco(const char *name = "CGL");
  virtual ~CglEmbedreco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  int copyWrapper(PHCompositeNode *topNode);
  mPHDchTrackModel* dchTrackModel;
  cglDetectorGeo* cglDetGeo;
  mPHLineTrack* cglLineTrack;
  cglHitAssociate* CglHitAssociate;
  int FieldOnFlag;
  TriggerHelper *TrigHelp;
};

#endif /* CGLEMBEDRECO_H__ */
