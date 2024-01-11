#ifndef __EMCEMBEDEVENTRECO_H__
#define __EMCEMBEDEVENTRECO_H__

#include "SubsysReco.h"

class EmcEmbedEventReco : public SubsysReco
{
 public:
  EmcEmbedEventReco();
  virtual ~EmcEmbedEventReco() {}

  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:
  bool fIsValid;
};

#endif
