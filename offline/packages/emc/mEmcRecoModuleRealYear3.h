#ifndef __mEmcRecoModuleRealYear3_h__
#define __mEmcRecoModuleRealYear3_h__

#include <SubsysRecoStack.h>

class PHFlag;
class PHTimeStamp;


/** Test class. 

The real stuff is mEmcRecoModuleRealYear3v1, really.

*/

class mEmcRecoModuleRealYear3 : public SubsysRecoStack
{
public:
  mEmcRecoModuleRealYear3(const PHFlag& flag);
  virtual ~mEmcRecoModuleRealYear3();

  using SubsysRecoStack::process_event;

  using SubsysRecoStack::End;
  
  const char* getName() const { return Name(); }

  int InitRun(PHCompositeNode* topNode);
  
private:
  mEmcRecoModuleRealYear3() {}
  mEmcRecoModuleRealYear3(const mEmcRecoModuleRealYear3&) {}
  mEmcRecoModuleRealYear3& operator=(const mEmcRecoModuleRealYear3&) 
  { return *this; }

private:

  PHTimeStamp* fTimeStamp;

  static const float fgPbScTowerThreshold;
  static const float fgPbGlTowerThreshold;

  static const float fgPbScMinClusterEnergy;
  static const float fgPbGlMinClusterEnergy;
};

#endif
