/* Automatically generated.  Do not edit. */
#include "mTofDCMModule.h"
#include "mTofDCM.h"
#include "PHIODataNode.h"

#include "dTofFEMWrapper.h"

typedef PHIODataNode<dTofFEMWrapper> dTofFEMNode_t;

#include "dTofDCMParWrapper.h"

typedef PHIODataNode<dTofDCMParWrapper> dTofDCMParNode_t;

#include "dTofDCMWrapper.h"

typedef PHIODataNode<dTofDCMWrapper> dTofDCMNode_t;

PHBoolean
mTofDCMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DTOFFEM_ST *d1;
  TABLE_HEAD_ST t2;
  DTOFDCMPAR_ST *d2;
  TABLE_HEAD_ST t3;
  DTOFDCM_ST *d3;

  dTofFEMNode_t* n1 = static_cast<dTofFEMNode_t*>(nl[0]);
  dTofDCMParNode_t* n2 = static_cast<dTofDCMParNode_t*>(nl[1]);
  dTofDCMNode_t* n3 = static_cast<dTofDCMNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = mtofdcm_(
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
