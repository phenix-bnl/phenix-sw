#ifndef __mEmcRecoModuleRealYear2_h__
#define __mEmcRecoModuleRealYear2_h__

#include <SubsysRecoStack.h>

class PHFlag;
class PHTimeStamp;

/** SubsysReco for real data, Run2.

This one was created after the Run2 production was made, so it's a backward
 compatible stuff only.
*/

class mEmcRecoModuleRealYear2 : public SubsysRecoStack
{
public:

  mEmcRecoModuleRealYear2(const PHFlag& flags);
  virtual ~mEmcRecoModuleRealYear2(){}

  using SubsysRecoStack::process_event;

  virtual int End(PHCompositeNode* topNode);
  
  virtual const char* getName() const { return Name(); }

  virtual int InitRun(PHCompositeNode* topNode);
  
private:
  mEmcRecoModuleRealYear2() {}
  mEmcRecoModuleRealYear2(const mEmcRecoModuleRealYear2&) {}
  mEmcRecoModuleRealYear2& operator=(const mEmcRecoModuleRealYear2&) 
  { return *this; }

  void setupAna(PHCompositeNode*);
  SubsysReco* setupClustering(PHCompositeNode*);

private:

  bool fPP;
  PHTimeStamp* fTimeStamp;

  static const float fgPbScTowerThreshold;
  static const float fgPbGlTowerThreshold;

  static const float fgPbScMinClusterEnergy;
  static const float fgPbGlMinClusterEnergy;
};

#endif
