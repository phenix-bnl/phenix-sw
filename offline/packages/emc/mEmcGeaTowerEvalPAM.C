/* Automatically generated.  Do not edit. */
#include "mEmcGeaTowerEvalModule.h"
#include "mEmcGeaTowerEval.h"
#include "PHIODataNode.h"

#include "dEmcGeometryWrapper.h"

typedef PHIODataNode<dEmcGeometryWrapper> dEmcGeometryNode_t;

#include "dEmcEventWrapper.h"

typedef PHIODataNode<dEmcEventWrapper> dEmcEventNode_t;

#include "dEmcCalibTowerWrapper.h"

typedef PHIODataNode<dEmcCalibTowerWrapper> dEmcCalibTowerNode_t;

#include "dEmcGeaTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTrackWrapper> dEmcGeaTrackNode_t;

#include "dEmcGeaTowerTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTowerTrackWrapper> dEmcGeaTowerTrackNode_t;

#include "dEmcGeaTowerEvalWrapper.h"

typedef PHIODataNode<dEmcGeaTowerEvalWrapper> dEmcGeaTowerEvalNode_t;

PHBoolean
mEmcGeaTowerEvalModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCGEOMETRY_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCEVENT_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCCALIBTOWER_ST *d3;
  TABLE_HEAD_ST t4;
  DEMCGEATRACK_ST *d4;
  TABLE_HEAD_ST t5;
  DEMCGEATOWERTRACK_ST *d5;
  TABLE_HEAD_ST t6;
  DEMCGEATOWEREVAL_ST *d6;

  dEmcGeometryNode_t* n1 = static_cast<dEmcGeometryNode_t*>(nl[0]);
  dEmcEventNode_t* n2 = static_cast<dEmcEventNode_t*>(nl[1]);
  dEmcCalibTowerNode_t* n3 = static_cast<dEmcCalibTowerNode_t*>(nl[2]);
  dEmcGeaTrackNode_t* n4 = static_cast<dEmcGeaTrackNode_t*>(nl[3]);
  dEmcGeaTowerTrackNode_t* n5 = static_cast<dEmcGeaTowerTrackNode_t*>(nl[4]);
  dEmcGeaTowerEvalNode_t* n6 = static_cast<dEmcGeaTowerEvalNode_t*>(nl[5]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();
  t3 = n3->getData()->TableHeader();
  d3 = n3->getData()->TableData();
  t4 = n4->getData()->TableHeader();
  d4 = n4->getData()->TableData();
  t5 = n5->getData()->TableHeader();
  d5 = n5->getData()->TableData();
  t6 = n6->getData()->TableHeader();
  d6 = n6->getData()->TableData();

  result = memcgeatowereval_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);
  n6->getData()->SetRowCount(t6.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
