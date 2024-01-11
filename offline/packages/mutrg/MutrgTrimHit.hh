#ifndef __MUTRGTRIMHIT__
#define __MUTRGTRIMHIT__

#include "SubsysReco.h"

class PHComposliteNode;

class MutrgTrimHit : public SubsysReco{
public:
  MutrgTrimHit(const char *name="MutrgTrimHit");
  virtual ~MutrgTrimHit(void){;}

  int process_event(PHCompositeNode *top_node);
  void SetWinNstpMutrg(int win){win_nstp_mutrg=win;}

protected:
  int win_nstp_mutrg; // store -win to +win strips : def = 3
};

#endif /* __MUTRGTRIMHIT__ */
