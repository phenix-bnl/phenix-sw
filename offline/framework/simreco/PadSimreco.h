#ifndef __PADSIMRECO_H__
#define __PADSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

class padInclBad ;
class padDetectorGeo ;
class padEvtToRaw ;
class PadRecModule ;
class mPadSlowSimModule ;
class dPadFEMParWrapper ;
class dPadSlowSimParWrapper ;
class mPadFEMModule ;
class mPadDCMModule ;
class dPadEvalParWrapper ;
class mPadEvaluateModule ;

class PadSimreco: public SubsysReco
{
 public:
  PadSimreco(const std::string &name = "PAD");
  virtual ~PadSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:

  // Utility functions
  int copyWrapper(PHCompositeNode *);
  void setPadSplitMode(const short padMode);
  short getPadSplitMode() const;


  //  Pointers to data
  padInclBad *PadInclBad ;
  padDetectorGeo *mPadDetGeo ;
  padEvtToRaw *PadEvtToRaw ;
  PadRecModule *Pc1Rec ;
  PadRecModule *Pc2Rec ;
  PadRecModule *Pc3Rec ;
  mPadSlowSimModule *mPc1SlowSim ;
  mPadSlowSimModule *mPc2SlowSim ;
  mPadSlowSimModule *mPc3SlowSim ;
  mPadFEMModule *mPc1FEM ;
  mPadFEMModule *mPc2FEM ;
  mPadFEMModule *mPc3FEM ;
  mPadDCMModule *mPc1DCM ;
  mPadDCMModule *mPc2DCM ;
  mPadDCMModule *mPc3DCM ;
  mPadEvaluateModule *mPc1Evaluate ;
  mPadEvaluateModule *mPc2Evaluate ;
  mPadEvaluateModule *mPc3Evaluate ;
  dPadFEMParWrapper *dPadFEMPar;
  dPadSlowSimParWrapper *dPadSlowSimPar;
  dPadEvalParWrapper *dPadEvalPar;
  short padSplitMode;  // default 0 means no splitting of clusters (Run2 v03 DSTs choice)

};

#endif /* __PADSIMRECO_H__ */
