#ifndef CGLRECO_H__
#define CGLRECO_H__

#include <SubsysReco.h>

class cglDetectorGeo;
class cglHitAssociate;
class mPHDchTrackModel;
class mPHLineTrack;
class PHCompositeNode;
class TriggerHelper;

class CglReco: public SubsysReco
{
 public:
  CglReco(const std::string &name = "CGL");
  virtual ~CglReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  void set_SvxUseAsciiFile(bool a) {SvxUseAsciiFile = a;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  int copyWrapper(PHCompositeNode *topNode);
  mPHDchTrackModel* dchTrackModel;
  cglDetectorGeo *CglDetGeo;
  mPHLineTrack* cglLineTrack;
  cglHitAssociate* CglHitAssociate;
  int FieldOnFlag;
  TriggerHelper *TrigHelp;
  bool SvxUseAsciiFile;
};

#endif /* CGLRECO_H__ */
