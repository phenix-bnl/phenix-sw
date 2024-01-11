#ifndef __PHMUOTRACKADCRECO__
#define __PHMUOTRACKADCRECO__

#include "SubsysReco.h"

class PHCompositeNode;

class PHMuoTrackAdcReco : public SubsysReco{
public:
  PHMuoTrackAdcReco(const char *name="PHMuoTrackAdcReco");
  virtual ~PHMuoTrackAdcReco(void){;}

  virtual int Init(PHCompositeNode *top_node);
  virtual int process_event(PHCompositeNode *top_node);

  virtual const char* ClassName(void){return Name();}
};

#endif /* __PHMUOTRACKADCRECO__ */
