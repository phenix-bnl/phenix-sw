/* Automatically generated.  Do not edit. */
#include "mTofAssocPidModule.h"
#include "mTofAssocPid.h"
#include "PHIODataNode.h"

#include "dTofPidParWrapper.h"

typedef PHIODataNode<dTofPidParWrapper> dTofPidParNode_t;

#include "dTofAssociateWrapper.h"

typedef PHIODataNode<dTofAssociateWrapper> dTofAssociateNode_t;

#include "dTofPidWrapper.h"

typedef PHIODataNode<dTofPidWrapper> dTofPidNode_t;

PHBoolean
mTofAssocPidModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DTOFPIDPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DTOFASSOCIATE_ST *d2;
  TABLE_HEAD_ST t3;
  DTOFPID_ST *d3;

  dTofPidParNode_t* n1 = static_cast<dTofPidParNode_t*>(nl[0]);
  dTofAssociateNode_t* n2 = static_cast<dTofAssociateNode_t*>(nl[1]);
  dTofPidNode_t* n3 = static_cast<dTofPidNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = mtofassocpid_(
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
