#include <Fun4AllReturnCodes.h>
#include "mEmcGeaGetHits.h"
#include "EmcGetGEA.h"

//_____________________________________________________________________________
mEmcGeaGetHits::mEmcGeaGetHits(): SubsysReco("mEmcGeaGetHits")
{
}

//_____________________________________________________________________________
mEmcGeaGetHits::~mEmcGeaGetHits()
{
}

//_____________________________________________________________________________
int
mEmcGeaGetHits::process_event(PHCompositeNode* top)
{
  ::EmcGetGEA(top);
  return EVENT_OK;
}
