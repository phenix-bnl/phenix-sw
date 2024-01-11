/* Automatically generated.  Do not edit. */
#include "mEmcPerfectModule.h"
#include "mEmcPerfect.h"
#include "PHIODataNode.h"

#include "dEmcGeaHitWrapper.h"

typedef PHIODataNode<dEmcGeaHitWrapper> dEmcGeaHitNode_t;

#include "dEmcClusterLocalWrapper.h"

typedef PHIODataNode<dEmcClusterLocalWrapper> dEmcClusterLocalNode_t;

#include "dEmcGeaClusterTrackWrapper.h"

typedef PHIODataNode<dEmcGeaClusterTrackWrapper> dEmcGeaClusterTrackNode_t;

PHBoolean
mEmcPerfectModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCGEAHIT_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCCLUSTERLOCAL_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCGEACLUSTERTRACK_ST *d3;

  dEmcGeaHitNode_t* n1 = static_cast<dEmcGeaHitNode_t*>(nl[0]);
  dEmcClusterLocalNode_t* n2 = static_cast<dEmcClusterLocalNode_t*>(nl[1]);
  dEmcGeaClusterTrackNode_t* n3 = static_cast<dEmcGeaClusterTrackNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = memcperfect_(
    &t1, d1,
    &t2, d2,
    &t3, d3                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
