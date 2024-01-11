/* Automatically generated.  Do not edit. */
#include "mBbcSetGeoModule.h"
#include "mBbcSetGeo.h"
#include "PHIODataNode.h"

#include "dBbcGeoWrapper.h"

typedef PHIODataNode<dBbcGeoWrapper> dBbcGeoNode_t;

PHBoolean
mBbcSetGeoModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DBBCGEO_ST *d1;

  dBbcGeoNode_t* n1 = static_cast<dBbcGeoNode_t*>(nl[0]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();

  result = mbbcsetgeo_(
    &t1, d1                              );

  n1->getData()->SetRowCount(t1.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
