/* Automatically generated.  Do not edit. */
#include "mTofFEMModule.h"
#include "mTofFEM.h"
#include "PHIODataNode.h"

#include "dTofRawWrapper.h"

typedef PHIODataNode<dTofRawWrapper> dTofRawNode_t;

#include "dTofGhitRawWrapper.h"

typedef PHIODataNode<dTofGhitRawWrapper> dTofGhitRawNode_t;

#include "dTofFEMmapWrapper.h"

typedef PHIODataNode<dTofFEMmapWrapper> dTofFEMmapNode_t;

#include "dTofFEMWrapper.h"

typedef PHIODataNode<dTofFEMWrapper> dTofFEMNode_t;

#include "dTofFEMhitGhitWrapper.h"

typedef PHIODataNode<dTofFEMhitGhitWrapper> dTofFEMhitGhitNode_t;

PHBoolean
mTofFEMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DTOFRAW_ST *d1;
  TABLE_HEAD_ST t2;
  DTOFGHITRAW_ST *d2;
  TABLE_HEAD_ST t3;
  DTOFFEMMAP_ST *d3;
  TABLE_HEAD_ST t4;
  DTOFFEM_ST *d4;
  TABLE_HEAD_ST t5;
  DTOFFEMHITGHIT_ST *d5;

  dTofRawNode_t* n1 = static_cast<dTofRawNode_t*>(nl[0]);
  dTofGhitRawNode_t* n2 = static_cast<dTofGhitRawNode_t*>(nl[1]);
  dTofFEMmapNode_t* n3 = static_cast<dTofFEMmapNode_t*>(nl[2]);
  dTofFEMNode_t* n4 = static_cast<dTofFEMNode_t*>(nl[3]);
  dTofFEMhitGhitNode_t* n5 = static_cast<dTofFEMhitGhitNode_t*>(nl[4]);

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

  result = mtoffem_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
