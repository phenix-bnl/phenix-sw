/* Automatically generated.  Do not edit. */
#include "mPadRecModule.h"
#include "mPadRec.h"
#include "PHIODataNode.h"

#include "dPadRecParWrapper.h"

typedef PHIODataNode<dPadRecParWrapper> dPadRecParNode_t;

#include "dPadGeomWrapper.h"

typedef PHIODataNode<dPadGeomWrapper> dPadGeomNode_t;

#include "dPadRawWrapper.h"

typedef PHIODataNode<dPadRawWrapper> dPadRawNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dPadRawClusWrapper.h"

typedef PHIODataNode<dPadRawClusWrapper> dPadRawClusNode_t;

#include "dPad23ParWrapper.h"

typedef PHIODataNode<dPad23ParWrapper> dPad23ParNode_t;

PHBoolean
mPadRecModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DPADRECPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DPADGEOM_ST *d2;
  TABLE_HEAD_ST t3;
  DPADRAW_ST *d3;
  TABLE_HEAD_ST t4;
  DPADCLUSTER_ST *d4;
  TABLE_HEAD_ST t5;
  DPADRAWCLUS_ST *d5;
  TABLE_HEAD_ST t6;
  DPAD23PAR_ST *d6;

  dPadRecParNode_t* n1 = static_cast<dPadRecParNode_t*>(nl[0]);
  dPadGeomNode_t* n2 = static_cast<dPadGeomNode_t*>(nl[1]);
  dPadRawNode_t* n3 = static_cast<dPadRawNode_t*>(nl[2]);
  dPadClusterNode_t* n4 = static_cast<dPadClusterNode_t*>(nl[3]);
  dPadRawClusNode_t* n5 = static_cast<dPadRawClusNode_t*>(nl[4]);
  dPad23ParNode_t* n6 = static_cast<dPad23ParNode_t*>(nl[5]);

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

  result = mpadrec_(
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
