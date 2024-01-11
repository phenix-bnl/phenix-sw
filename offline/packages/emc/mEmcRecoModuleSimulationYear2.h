#ifndef __mEmcRecoModuleSimulationYear2_h__
#define __mEmcRecoModuleSimulationYear2_h__

#include "emcRecoModule.h"
#include "emcModuleHelper.h"
#include "PHTimeStamp.h"

class PHFlag;
class dEmcRespParWrapper;
class PHCompositeNode;

/** emcRecoModule for simulated data, Run2. 

\note
This module has been written "after the fact", and as such should be considered
 only as a backward-compatibility issue.
\endnote

*/
class mEmcRecoModuleSimulationYear2 : public emcRecoModule
{

public:

  mEmcRecoModuleSimulationYear2(const PHFlag& flags);
  virtual ~mEmcRecoModuleSimulationYear2(){}

  virtual int ana(PHCompositeNode*);
  virtual int end(PHCompositeNode*);
  virtual const char* getName() const 
  { return "mEmcRecoModuleSimulationYear2"; }
  virtual int setup(PHCompositeNode*);

private:

  void setupAna(PHCompositeNode*);  
  EMCModule* setupClustering(PHCompositeNode*);
  void setupEvaluation(PHCompositeNode*);
  void setupResponse(dEmcRespParWrapper*);

private:

  bool fEvaluation;
  int fSimulationFlag;
  bool fPP;

  static const float fgPbScTowerThreshold;
  static const float fgPbGlTowerThreshold;

  static const float fgPbScMinClusterEnergy;
  static const float fgPbGlMinClusterEnergy;

  PHTimeStamp fTimeStamp;
};
#endif
