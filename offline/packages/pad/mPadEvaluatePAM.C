/* Automatically generated.  Do not edit. */
#include "mPadEvaluateModule.h"
#include "mPadEvaluate.h"
#include "PHIODataNode.h"

#include "dPadEvalParWrapper.h"

typedef PHIODataNode<dPadEvalParWrapper> dPadEvalParNode_t;

#include "dPadGeomWrapper.h"

typedef PHIODataNode<dPadGeomWrapper> dPadGeomNode_t;

#include "pcghitWrapper.h"

typedef PHIODataNode<pcghitWrapper> pcghitNode_t;

#include "dPadRawWrapper.h"

typedef PHIODataNode<dPadRawWrapper> dPadRawNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dPadGhitRawWrapper.h"

typedef PHIODataNode<dPadGhitRawWrapper> dPadGhitRawNode_t;

#include "dPadRawClusWrapper.h"

typedef PHIODataNode<dPadRawClusWrapper> dPadRawClusNode_t;

#include "dPadGhitClusWrapper.h"

typedef PHIODataNode<dPadGhitClusWrapper> dPadGhitClusNode_t;

#include "dPadEvalWrapper.h"

typedef PHIODataNode<dPadEvalWrapper> dPadEvalNode_t;

PHBoolean
mPadEvaluateModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DPADEVALPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DPADGEOM_ST *d2;
  TABLE_HEAD_ST t3;
  PCGHIT_ST *d3;
  TABLE_HEAD_ST t4;
  DPADRAW_ST *d4;
  TABLE_HEAD_ST t5;
  DPADCLUSTER_ST *d5;
  TABLE_HEAD_ST t6;
  DPADGHITRAW_ST *d6;
  TABLE_HEAD_ST t7;
  DPADRAWCLUS_ST *d7;
  TABLE_HEAD_ST t8;
  DPADGHITCLUS_ST *d8;
  TABLE_HEAD_ST t9;
  DPADEVAL_ST *d9;

  dPadEvalParNode_t* n1 = static_cast<dPadEvalParNode_t*>(nl[0]);
  dPadGeomNode_t* n2 = static_cast<dPadGeomNode_t*>(nl[1]);
  pcghitNode_t* n3 = static_cast<pcghitNode_t*>(nl[2]);
  dPadRawNode_t* n4 = static_cast<dPadRawNode_t*>(nl[3]);
  dPadClusterNode_t* n5 = static_cast<dPadClusterNode_t*>(nl[4]);
  dPadGhitRawNode_t* n6 = static_cast<dPadGhitRawNode_t*>(nl[5]);
  dPadRawClusNode_t* n7 = static_cast<dPadRawClusNode_t*>(nl[6]);
  dPadGhitClusNode_t* n8 = static_cast<dPadGhitClusNode_t*>(nl[7]);
  dPadEvalNode_t* n9 = static_cast<dPadEvalNode_t*>(nl[8]);

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
  t7 = n7->getData()->TableHeader();
  d7 = n7->getData()->TableData();
  t8 = n8->getData()->TableHeader();
  d8 = n8->getData()->TableData();
  t9 = n9->getData()->TableHeader();
  d9 = n9->getData()->TableData();

  result = mpadevaluate_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6,
    &t7, d7,
    &t8, d8,
    &t9, d9                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);
  n6->getData()->SetRowCount(t6.nok);
  n7->getData()->SetRowCount(t7.nok);
  n8->getData()->SetRowCount(t8.nok);
  n9->getData()->SetRowCount(t9.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
