/* Automatically generated.  Do not edit. */
#include "mEmcGeaTrackModule.h"
#include "mEmcGeaTrack.h"
#include "PHIODataNode.h"

#include "dEmcGeaTrackTowerWrapper.h"

typedef PHIODataNode<dEmcGeaTrackTowerWrapper> dEmcGeaTrackTowerNode_t;

#include "dEmcGeaTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTrackWrapper> dEmcGeaTrackNode_t;

PHBoolean
mEmcGeaTrackModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCGEATRACKTOWER_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEATRACK_ST *d2;

  dEmcGeaTrackTowerNode_t* n1 = static_cast<dEmcGeaTrackTowerNode_t*>(nl[0]);
  dEmcGeaTrackNode_t* n2 = static_cast<dEmcGeaTrackNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcgeatrack_(
    &t1, d1,
    &t2, d2                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
