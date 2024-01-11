/* Automatically generated.  Do not edit. */
#include "mTofSetFEMmapModule.h"
#include "mTofSetFEMmap.h"
#include "PHIODataNode.h"

#include "dTofGeoWrapper.h"

typedef PHIODataNode<dTofGeoWrapper> dTofGeoNode_t;

#include "dTofFEMmapWrapper.h"

typedef PHIODataNode<dTofFEMmapWrapper> dTofFEMmapNode_t;

PHBoolean
mTofSetFEMmapModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DTOFGEO_ST *d1;
  TABLE_HEAD_ST t2;
  DTOFFEMMAP_ST *d2;

  dTofGeoNode_t* n1 = static_cast<dTofGeoNode_t*>(nl[0]);
  dTofFEMmapNode_t* n2 = static_cast<dTofFEMmapNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = mtofsetfemmap_(
    &t1, d1,
    &t2, d2                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
