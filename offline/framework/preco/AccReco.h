#ifndef __ACCRECO_H_
#define __ACCRECO_H_


#include <SubsysReco.h>

class AccEvent;
class AccGeometry;
class AccHit;
class AccRaw;

class AccReco : public SubsysReco
{

 public:
  AccReco(const std::string &name = "AccReco", const int ver=1);
  virtual ~AccReco();

  int Init(PHCompositeNode* topNode);
  int InitRun(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);
  int ResetEvent(PHCompositeNode* topNode);

 protected:
  int version;

  AccRaw* d_raw;
  AccHit* d_hit;
  AccEvent* d_acc;
  AccGeometry* d_geo;

  bool CreateNodeTree(PHCompositeNode* topNode);

};

#endif
