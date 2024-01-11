/* Automatically generated.  Do not edit. */
#include "mEmcDefGeomModule.h"
#include "mEmcDefGeom.h"
#include "PHIODataNode.h"

#include "dEmcGeaParamsWrapper.h"

typedef PHIODataNode<dEmcGeaParamsWrapper> dEmcGeaParamsNode_t;

#include "dEmcGeometryWrapper.h"

typedef PHIODataNode<dEmcGeometryWrapper> dEmcGeometryNode_t;

PHBoolean
mEmcDefGeomModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCGEAPARAMS_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEOMETRY_ST *d2;

  dEmcGeaParamsNode_t* n1 = static_cast<dEmcGeaParamsNode_t*>(nl[0]);
  dEmcGeometryNode_t* n2 = static_cast<dEmcGeometryNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcdefgeom_(
    &t1, d1,
    &t2, d2   );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
