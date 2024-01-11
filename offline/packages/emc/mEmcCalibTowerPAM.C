/* Automatically generated.  Do not edit. */
#include "mEmcCalibTowerModule.h"
#include "mEmcCalibTower.h"
#include "PHIODataNode.h"

#include "dEmcRawDataWrapper.h"

typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;

#include "dEmcGeometryWrapper.h"

typedef PHIODataNode<dEmcGeometryWrapper> dEmcGeometryNode_t;

#include "dEmcEventWrapper.h"

typedef PHIODataNode<dEmcEventWrapper> dEmcEventNode_t;

#include "dEmcCalibTowerWrapper.h"

typedef PHIODataNode<dEmcCalibTowerWrapper> dEmcCalibTowerNode_t;

PHBoolean
mEmcCalibTowerModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCRAWDATA_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEOMETRY_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCEVENT_ST *d3;
  TABLE_HEAD_ST t4;
  DEMCCALIBTOWER_ST *d4;

  dEmcRawDataNode_t* n1 = static_cast<dEmcRawDataNode_t*>(nl[0]);
  dEmcGeometryNode_t* n2 = static_cast<dEmcGeometryNode_t*>(nl[1]);
  dEmcEventNode_t* n3 = static_cast<dEmcEventNode_t*>(nl[2]);
  dEmcCalibTowerNode_t* n4 = static_cast<dEmcCalibTowerNode_t*>(nl[3]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();
  t4 = n4->getData()->TableHeader();
  d4 = n4->getData()->TableData();

  result = memccalibtower_(
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
