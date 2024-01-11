/* Automatically generated.  Do not edit. */
#include "mEmcGeaParamsModule.h"
#include "mEmcGeaParams.h"
#include "PHIODataNode.h"

#include "emcparWrapper.h"

typedef PHIODataNode<emcparWrapper> emcparNode_t;

#include "dEmcGeaParamsWrapper.h"

typedef PHIODataNode<dEmcGeaParamsWrapper> dEmcGeaParamsNode_t;

#include "dEmcGeometryWrapper.h"

typedef PHIODataNode<dEmcGeometryWrapper> dEmcGeometryNode_t;

PHBoolean
mEmcGeaParamsModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  EMCPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEAPARAMS_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCGEOMETRY_ST *d3;

  emcparNode_t* n1 = static_cast<emcparNode_t*>(nl[0]);
  dEmcGeaParamsNode_t* n2 = static_cast<dEmcGeaParamsNode_t*>(nl[1]);
  dEmcGeometryNode_t* n3 = static_cast<dEmcGeometryNode_t*>(nl[2]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();

  result = memcgeaparams_(
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
