#ifndef __HBDRECO_H__
#define __HBDRECO_H__

#include <SubsysReco.h>

class HbdDcmRaw;
class hbdDetectorGeo;

class HbdReco: public SubsysReco
{
 public:
  HbdReco(const std::string &name = "HBD");
  virtual ~HbdReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  HbdDcmRaw *d_dcm;
  hbdDetectorGeo *geo;

  //Private methods for "utility" functions...
  int CreateNodeTree(PHCompositeNode *topNode);
};

#endif /* __HBDRECO_H__ */
