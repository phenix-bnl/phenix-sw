#include <Fun4AllReturnCodes.h>
#include "mEmcGetDCMModule.h"
#include "PHCompositeNode.h"

extern long EmcGetDCM(PHCompositeNode*);

//_____________________________________________________________________________
mEmcGetDCMModule::mEmcGetDCMModule(): SubsysReco("mEmcGetDCMModule")
{
}

int
mEmcGetDCMModule::process_event(PHCompositeNode* topNode)
{
  ::EmcGetDCM(topNode);
  return EVENT_OK;
}
