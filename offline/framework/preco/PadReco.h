#ifndef __PADRECO_H__
#define __PADRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class padEvtToRaw;
class padInclBad;
class padDetectorGeo;
class PadRecModule;

class PadReco: public SubsysReco
{
 public:
  PadReco(const std::string &name = "PAD");
  virtual ~PadReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  void NoRawDataCheck(const int i = 1) {norawdatacheck = i;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  int copyWrapper(PHCompositeNode *topNode);

  padEvtToRaw *PadEvtToRaw;
  padInclBad *PadInclBad;
  padDetectorGeo *mPadDetGeo;
  PadRecModule *Pc1Rec;
  PadRecModule *Pc2Rec;
  PadRecModule *Pc3Rec;
  short int padSplitMode;
  int norawdatacheck;
};

#endif /* __PADRECO_H__ */
