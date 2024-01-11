#include <Fun4AllReturnCodes.h>
#include "mEmcPutDCMModule.h"
#include "PHCompositeNode.h"

extern long EmcPutDCM(PHCompositeNode* topNode);

//_____________________________________________________________________________
mEmcPutDCMModule::mEmcPutDCMModule(): SubsysReco("mEmcPutDCMModule")
{
}

int
mEmcPutDCMModule::process_event(PHCompositeNode* topNode)
{
  ::EmcPutDCM(topNode);
  return EVENT_OK;
}
