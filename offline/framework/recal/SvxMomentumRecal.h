#ifndef __SVXMOMENTUMRECAL__
#define __SVXMOMENTUMRECAL__


class PHCentralTrack;
class PHCompositeNode;
class SvxCentralTrackList;
class SvxCentralTrackRecalList;

#include "SubsysReco.h"
#include <vector>


class SvxMomentumRecal : public SubsysReco
{
public:
  SvxMomentumRecal();
  ~SvxMomentumRecal();
  
  int Init(PHCompositeNode *topNode){return 0;}
  int InitRun(PHCompositeNode *topNode){return 0;}
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode){return 0;};
  int getNodes(PHCompositeNode *topNode);
  
  PHCentralTrack* centraltrack;
  SvxCentralTrackList* svxcentraltrack;
  SvxCentralTrackRecalList* svxcentraltrackrecal;
  
  void setSimulationFlag(bool is_sim){is_simulation=is_sim;}
  
  bool is_simulation;
};

#endif
