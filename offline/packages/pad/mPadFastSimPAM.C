/* Automatically generated.  Do not edit. */
#include "mPadFastSimModule.h"
#include "mPadFastSim.h"
#include "PHIODataNode.h"

#include "pcghitWrapper.h"

typedef PHIODataNode<pcghitWrapper> pcghitNode_t;

#include "dPadFastSimParWrapper.h"

typedef PHIODataNode<dPadFastSimParWrapper> dPadFastSimParNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dPadGhitClusWrapper.h"

typedef PHIODataNode<dPadGhitClusWrapper> dPadGhitClusNode_t;

PHBoolean
mPadFastSimModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  PCGHIT_ST *d1;
  TABLE_HEAD_ST t2;
  DPADFASTSIMPAR_ST *d2;
  TABLE_HEAD_ST t3;
  DPADCLUSTER_ST *d3;
  TABLE_HEAD_ST t4;
  DPADGHITCLUS_ST *d4;

  pcghitNode_t* n1 = static_cast<pcghitNode_t*>(nl[0]);
  dPadFastSimParNode_t* n2 = static_cast<dPadFastSimParNode_t*>(nl[1]);
  dPadClusterNode_t* n3 = static_cast<dPadClusterNode_t*>(nl[2]);
  dPadGhitClusNode_t* n4 = static_cast<dPadGhitClusNode_t*>(nl[3]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();
  t4 = n4->getData()->TableHeader();
  d4 = n4->getData()->TableData();

  result = mpadfastsim_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
