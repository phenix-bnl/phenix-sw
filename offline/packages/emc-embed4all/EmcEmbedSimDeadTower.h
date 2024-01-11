#ifndef __EMCEMBEDSIMDEADTOWER_H__
#define __EMCEMBEDSIMDEADTOWER_H__

#include "SubsysReco.h"
#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#include <string>

class emcBadModules;

/** Get the list of dead towers from the real events,
    and sets the simulated tower energy to zero for the dead
    towers.
*/

class EmcEmbedSimDeadTower : public SubsysReco
{
 public:

  EmcEmbedSimDeadTower(const char* realNode="REAL",
		       const char* simuNode="SIMU");
		       

  virtual ~EmcEmbedSimDeadTower();

  int Init(PHCompositeNode*);

  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:

  int my_process_event(PHCompositeNode*);

  emcManageable::EStorage fDataSource;
  emcBadModules* fBadModules;
  std::string fRealNodeName;
  std::string fSimuNodeName;
};

#endif
