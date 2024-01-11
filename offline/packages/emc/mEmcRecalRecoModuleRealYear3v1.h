#ifndef __mEmcRecalRecoModuleRealYear3v1_h__
#define __mEmcRecalRecoModuleRealYear3v1_h__

#include <SubsysRecoStack.h>
#include <string>

class PHTimeStamp;
class PHFlag;

class mEmcRecalRecoModuleRealYear3v1 : public SubsysRecoStack
{
public:
  
  mEmcRecalRecoModuleRealYear3v1(const PHFlag&);

  virtual ~mEmcRecalRecoModuleRealYear3v1();

  using SubsysRecoStack::process_event;

  using SubsysRecoStack::End;

  const char* getName() const { return Name(); }

  int InitRun(PHCompositeNode* topNode); 

  virtual void identify(std::ostream& os = std::cout) const;

  virtual int isValid() const { return 1; }

private:

  void createNodeTree(PHCompositeNode* topNode);
  void setup_calibrator(PHCompositeNode* topNode);
  void setup_clustering(PHCompositeNode* topNode);

private:
  PHTimeStamp* fTimeStamp;
  int fRunNumber;
  std::string fDstNodeName;
  bool fConstantGains;
  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;
};

#endif
