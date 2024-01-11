/* Automatically generated.  Do not edit. */
#include "mPadFEMModule.h"
#include "mPadFEM.h"
#include "PHIODataNode.h"

#include "dPadRawWrapper.h"

typedef PHIODataNode<dPadRawWrapper> dPadRawNode_t;

#include "dPadGhitRawWrapper.h"

typedef PHIODataNode<dPadGhitRawWrapper> dPadGhitRawNode_t;

#include "dPadGeomWrapper.h"

typedef PHIODataNode<dPadGeomWrapper> dPadGeomNode_t;

#include "dPadFEMParWrapper.h"

typedef PHIODataNode<dPadFEMParWrapper> dPadFEMParNode_t;

#include "dPadFEMWrapper.h"

typedef PHIODataNode<dPadFEMWrapper> dPadFEMNode_t;

#include "dPadNibbleGhitWrapper.h"

typedef PHIODataNode<dPadNibbleGhitWrapper> dPadNibbleGhitNode_t;

PHBoolean
mPadFEMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DPADRAW_ST *d1;
  TABLE_HEAD_ST t2;
  DPADGHITRAW_ST *d2;
  TABLE_HEAD_ST t3;
  DPADGEOM_ST *d3;
  TABLE_HEAD_ST t4;
  DPADFEMPAR_ST *d4;
  TABLE_HEAD_ST t5;
  DPADFEM_ST *d5;
  TABLE_HEAD_ST t6;
  DPADNIBBLEGHIT_ST *d6;

  dPadRawNode_t* n1 = static_cast<dPadRawNode_t*>(nl[0]);
  dPadGhitRawNode_t* n2 = static_cast<dPadGhitRawNode_t*>(nl[1]);
  dPadGeomNode_t* n3 = static_cast<dPadGeomNode_t*>(nl[2]);
  dPadFEMParNode_t* n4 = static_cast<dPadFEMParNode_t*>(nl[3]);
  dPadFEMNode_t* n5 = static_cast<dPadFEMNode_t*>(nl[4]);
  dPadNibbleGhitNode_t* n6 = static_cast<dPadNibbleGhitNode_t*>(nl[5]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();
  t4 = n4->getData()->TableHeader();
  d4 = n4->getData()->TableData();
  t5 = n5->getData()->TableHeader();
  d5 = n5->getData()->TableData();
  t6 = n6->getData()->TableHeader();
  d6 = n6->getData()->TableData();

  result = mpadfem_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);
  n6->getData()->SetRowCount(t6.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
