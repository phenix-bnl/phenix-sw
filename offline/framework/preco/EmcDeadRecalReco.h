#ifndef __EMCDEADRECALRECO_H__
#define __EMCDEADRECALRECO_H__

#include "SubsysReco.h"
#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#include <string>

class emcBadModules;

/** Module to (re-)apply dead and warnmap to emc clusters.
 */

class EmcDeadRecalReco : public SubsysReco
{
 public:

  EmcDeadRecalReco(const char* topNode="TOP", const char* dataNode="emcClusterContainer");

  virtual ~EmcDeadRecalReco();

  int Init(PHCompositeNode*);

  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:

  int my_process_event(PHCompositeNode*);

  emcManageable::EStorage fDataSource;
  emcBadModules* fBadModules;
  std::string fTopNodeName;
  std::string fDataNodeName;
};

#endif
