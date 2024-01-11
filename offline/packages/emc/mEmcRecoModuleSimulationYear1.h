#ifndef __mEmcRecoModuleSimulationYear1_h__
#define __mEmcRecoModuleSimulationYear1_h__

#include <SubsysRecoStack.h>

class PHFlag;

class dEmcRespParWrapper;

/** SubsysReco for simulated data, Run1. 

\note
This module has been written "after the fact", and as such should be considered
 only as a backward-compatibility issue.
\endnote

*/

class PHCompositeNode;

class mEmcRecoModuleSimulationYear1 : public SubsysRecoStack
{

public:

  mEmcRecoModuleSimulationYear1(const PHFlag& flags);
  virtual ~mEmcRecoModuleSimulationYear1(){}

  using SubsysRecoStack::process_event;

  using SubsysRecoStack::End;

  virtual const char* getName() const { return Name(); }

  virtual int InitRun(PHCompositeNode*);

private:

  void setupAna(PHCompositeNode*);
  SubsysReco * setupClustering(PHCompositeNode*);
  void setupEvaluation(PHCompositeNode*);
  void setupResponse(dEmcRespParWrapper*);

private:

  bool fEvaluation;
  int fSimulationFlag;

  static const float fgPbScTowerThreshold;
  static const float fgPbGlTowerThreshold;

  static const float fgPbScMinClusterEnergy;
  static const float fgPbGlMinClusterEnergy;


};
#endif
