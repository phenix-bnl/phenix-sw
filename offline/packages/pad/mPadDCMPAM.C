/* Automatically generated.  Do not edit. */
#include "mPadDCMModule.h"
#include "mPadDCM.h"
#include "PHIODataNode.h"

#include "dPadFEMWrapper.h"

typedef PHIODataNode<dPadFEMWrapper> dPadFEMNode_t;

#include "dPadDCMParWrapper.h"

typedef PHIODataNode<dPadDCMParWrapper> dPadDCMParNode_t;

#include "dPadDCMWrapper.h"

typedef PHIODataNode<dPadDCMWrapper> dPadDCMNode_t;

PHBoolean
mPadDCMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DPADFEM_ST *d1;
  TABLE_HEAD_ST t2;
  DPADDCMPAR_ST *d2;
  TABLE_HEAD_ST t3;
  DPADDCM_ST *d3;

  dPadFEMNode_t* n1 = static_cast<dPadFEMNode_t*>(nl[0]);
  dPadDCMParNode_t* n2 = static_cast<dPadDCMParNode_t*>(nl[1]);
  dPadDCMNode_t* n3 = static_cast<dPadDCMNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = mpaddcm_(
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
