#ifndef __Lvl2PrimRun4Reco_H_
#define __Lvl2PrimRun4Reco_H_

#include <SubsysReco.h>

class PHCompositeNode;

class Lvl2PrimRun4Reco: public SubsysReco
{
 private:
  int process_lvl2ReadBackRun4(PHCompositeNode* topNode);
  int process_L2TrackEMCLowOcupy(PHCompositeNode* topNode);
  int process_L2TrackPC3LowOcupy(PHCompositeNode* topNode);
  int process_L2ElecCanddtLowOcupy(PHCompositeNode* topNode);
  int process_L2ElecInvMassLowOcupy(PHCompositeNode* topNode);
  int process_L2PC1PC3TrackEMCAssoc(PHCompositeNode* topNode);
 public:
  Lvl2PrimRun4Reco();
  virtual ~Lvl2PrimRun4Reco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const char *what) const {return;}
  const char *Name() const
    {
      return ThisName.c_str();
    }

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __Lvl2PrimRun4Reco_H__ */
