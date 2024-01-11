/* Automatically generated.  Do not edit. */
#include "mPadDumpRawModule.h"
#include "mPadDumpRaw.h"
#include "PHIODataNode.h"

#include "dPadRawWrapper.h"

typedef PHIODataNode<dPadRawWrapper> dPadRawNode_t;

#include "dPadGhitRawWrapper.h"

typedef PHIODataNode<dPadGhitRawWrapper> dPadGhitRawNode_t;

#include "dPadFEMParWrapper.h"

typedef PHIODataNode<dPadFEMParWrapper> dPadFEMParNode_t;

PHBoolean
mPadDumpRawModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DPADRAW_ST *d1;
  TABLE_HEAD_ST t2;
  DPADGHITRAW_ST *d2;
  TABLE_HEAD_ST t3;
  DPADFEMPAR_ST *d3;

  dPadRawNode_t* n1 = static_cast<dPadRawNode_t*>(nl[0]);
  dPadGhitRawNode_t* n2 = static_cast<dPadGhitRawNode_t*>(nl[1]);
  dPadFEMParNode_t* n3 = static_cast<dPadFEMParNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = mpaddumpraw_(
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
