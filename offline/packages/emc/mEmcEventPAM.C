/* Automatically generated.  Do not edit. */
#include "mEmcEventModule.h"
#include "mEmcEvent.h"
#include "PHIODataNode.h"

#include "dBbcOutWrapper.h"

typedef PHIODataNode<dBbcOutWrapper> dBbcOutNode_t;

#include "dEmcEventWrapper.h"

typedef PHIODataNode<dEmcEventWrapper> dEmcEventNode_t;

PHBoolean
mEmcEventModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DBBCOUT_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCEVENT_ST *d2;

  dBbcOutNode_t* n1 = static_cast<dBbcOutNode_t*>(nl[0]);
  dEmcEventNode_t* n2 = static_cast<dEmcEventNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcevent_(
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
