#ifndef __LVL2SIMRECO_H__
#define __LVL2SIMRECO_H__

#include "SubsysReco.h"
#include <string>

class PHCompositeNode;
class Lvl2Event;

class Lvl2Simreco: public SubsysReco
{
 public:
  Lvl2Simreco(const std::string &name = "LVL2");
  virtual ~Lvl2Simreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  Lvl2Event *mLvl2Event;
};

#endif /* __LVL2SIMRECO_H__ */
