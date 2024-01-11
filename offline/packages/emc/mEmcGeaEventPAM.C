/* Automatically generated.  Do not edit. */
#include "mEmcGeaEventModule.h"
#include "mEmcGeaEvent.h"
#include "PHIODataNode.h"

#include "headerWrapper.h"

typedef PHIODataNode<headerWrapper> headerNode_t;

#include "dEmcEventWrapper.h"

typedef PHIODataNode<dEmcEventWrapper> dEmcEventNode_t;

PHBoolean
mEmcGeaEventModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  HEADER_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCEVENT_ST *d2;

  headerNode_t* n1 = static_cast<headerNode_t*>(nl[0]);
  dEmcEventNode_t* n2 = static_cast<dEmcEventNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcgeaevent_(
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
