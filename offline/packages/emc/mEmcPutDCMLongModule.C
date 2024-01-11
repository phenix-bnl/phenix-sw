#include <Fun4AllReturnCodes.h>
#include "mEmcPutDCMLongModule.h"
#include "PHCompositeNode.h"

extern long EmcPutDCMLong(PHCompositeNode* topNode);

//_____________________________________________________________________________
mEmcPutDCMLongModule::mEmcPutDCMLongModule(): SubsysReco("mEmcPutDCMLongModule")
{
}

int
mEmcPutDCMLongModule::process_event(PHCompositeNode* topNode)
{
  ::EmcPutDCMLong(topNode);
  return EVENT_OK;
}
