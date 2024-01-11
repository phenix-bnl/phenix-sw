#ifndef __LVL2RECO_H__
#define __LVL2RECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class Lvl2Event;

class Lvl2Reco: public SubsysReco
{
 public:
  Lvl2Reco(const std::string &name = "LVL2RECO");
  virtual ~Lvl2Reco();

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  int MakeLvl2TrigRunCal(PHCompositeNode *topNode);
  Lvl2Event *mLvl2Event;
  int nevt;
  int initdone;
  int lvl2copyonly;
};

#endif /* __LVL2RECO_H__ */
