/* Automatically generated.  Do not edit. */
#include "mEmcGeaMakeRawModule.h"
#include "mEmcGeaMakeRaw.h"
#include "PHIODataNode.h"

#include "headerWrapper.h"

typedef PHIODataNode<headerWrapper> headerNode_t;

#include "dEmcGeaHitWrapper.h"

typedef PHIODataNode<dEmcGeaHitWrapper> dEmcGeaHitNode_t;

#include "dEmcGeaParamsWrapper.h"

typedef PHIODataNode<dEmcGeaParamsWrapper> dEmcGeaParamsNode_t;

#include "dEmcRespParWrapper.h"

typedef PHIODataNode<dEmcRespParWrapper> dEmcRespParNode_t;

#include "dEmcGeometryWrapper.h"

typedef PHIODataNode<dEmcGeometryWrapper> dEmcGeometryNode_t;

#include "dEmcGeaTrackTowerWrapper.h"

typedef PHIODataNode<dEmcGeaTrackTowerWrapper> dEmcGeaTrackTowerNode_t;

#include "dEmcGeaTowerTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTowerTrackWrapper> dEmcGeaTowerTrackNode_t;

#include "dEmcRawDataWrapper.h"

typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;

PHBoolean
mEmcGeaMakeRawModule::callPAM(float r_lowgain_convfac, float r_highgain_convfac, PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  HEADER_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEAHIT_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCGEAPARAMS_ST *d3;
  TABLE_HEAD_ST t4;
  DEMCRESPPAR_ST *d4;
  TABLE_HEAD_ST t5;
  DEMCGEOMETRY_ST *d5;
  TABLE_HEAD_ST t6;
  DEMCGEATRACKTOWER_ST *d6;
  TABLE_HEAD_ST t7;
  DEMCGEATOWERTRACK_ST *d7;
  TABLE_HEAD_ST t8;
  DEMCRAWDATA_ST *d8;

  headerNode_t* n1 = static_cast<headerNode_t*>(nl[0]);
  dEmcGeaHitNode_t* n2 = static_cast<dEmcGeaHitNode_t*>(nl[1]);
  dEmcGeaParamsNode_t* n3 = static_cast<dEmcGeaParamsNode_t*>(nl[2]);
  dEmcRespParNode_t* n4 = static_cast<dEmcRespParNode_t*>(nl[3]);
  dEmcGeometryNode_t* n5 = static_cast<dEmcGeometryNode_t*>(nl[4]);
  dEmcGeaTrackTowerNode_t* n6 = static_cast<dEmcGeaTrackTowerNode_t*>(nl[5]);
  dEmcGeaTowerTrackNode_t* n7 = static_cast<dEmcGeaTowerTrackNode_t*>(nl[6]);
  dEmcRawDataNode_t* n8 = static_cast<dEmcRawDataNode_t*>(nl[7]);

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
  t7 = n7->getData()->TableHeader();
  d7 = n7->getData()->TableData();
  t8 = n8->getData()->TableHeader();
  d8 = n8->getData()->TableData();

  result = memcgeamakeraw_(
    r_lowgain_convfac, r_highgain_convfac,
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6,
    &t7, d7,
    &t8, d8                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);
  n6->getData()->SetRowCount(t6.nok);
  n7->getData()->SetRowCount(t7.nok);
  n8->getData()->SetRowCount(t8.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
