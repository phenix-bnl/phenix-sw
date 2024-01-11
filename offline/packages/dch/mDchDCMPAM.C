/* Automatically generated.  Do not edit. */
#include "mDchDCMModule.h"
#include "mDchDCM.h"
#include "PHIODataNode.h"

#include "dDchFEMWrapper.h"

typedef PHIODataNode<dDchFEMWrapper> dDchFEMNode_t;

#include "dDchDCMParWrapper.h"

typedef PHIODataNode<dDchDCMParWrapper> dDchDCMParNode_t;

#include "dDchDCMWrapper.h"

typedef PHIODataNode<dDchDCMWrapper> dDchDCMNode_t;

PHBoolean
mDchDCMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DDCHFEM_ST *d1;
  TABLE_HEAD_ST t2;
  DDCHDCMPAR_ST *d2;
  TABLE_HEAD_ST t3;
  DDCHDCM_ST *d3;

  dDchFEMNode_t* n1 = static_cast<dDchFEMNode_t*>(nl[0]);
  dDchDCMParNode_t* n2 = static_cast<dDchDCMParNode_t*>(nl[1]);
  dDchDCMNode_t* n3 = static_cast<dDchDCMNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = mdchdcm_(
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
