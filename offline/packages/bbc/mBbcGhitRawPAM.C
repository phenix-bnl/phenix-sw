/* Automatically generated.  Do not edit. */
#include "mBbcGhitRawModule.h"
#include "mBbcGhitRaw.h"
#include "PHIODataNode.h"

#include "dBbcGhitRawParWrapper.h"

typedef PHIODataNode<dBbcGhitRawParWrapper> dBbcGhitRawParNode_t;

#include "dBbcGeoWrapper.h"

typedef PHIODataNode<dBbcGeoWrapper> dBbcGeoNode_t;

#include "dBbcUcalWrapper.h"

typedef PHIODataNode<dBbcUcalWrapper> dBbcUcalNode_t;

#include "bbcghitWrapper.h"

typedef PHIODataNode<bbcghitWrapper> bbcghitNode_t;

#include "dBbcGhitRawWrapper.h"

typedef PHIODataNode<dBbcGhitRawWrapper> dBbcGhitRawNode_t;

#include "dBbcRawWrapper.h"

typedef PHIODataNode<dBbcRawWrapper> dBbcRawNode_t;

PHBoolean
mBbcGhitRawModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DBBCGHITRAWPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DBBCGEO_ST *d2;
  TABLE_HEAD_ST t3;
  DBBCUCAL_ST *d3;
  TABLE_HEAD_ST t4;
  BBCGHIT_ST *d4;
  TABLE_HEAD_ST t5;
  DBBCGHITRAW_ST *d5;
  TABLE_HEAD_ST t6;
  DBBCRAW_ST *d6;

  dBbcGhitRawParNode_t* n1 = static_cast<dBbcGhitRawParNode_t*>(nl[0]);
  dBbcGeoNode_t* n2 = static_cast<dBbcGeoNode_t*>(nl[1]);
  dBbcUcalNode_t* n3 = static_cast<dBbcUcalNode_t*>(nl[2]);
  bbcghitNode_t* n4 = static_cast<bbcghitNode_t*>(nl[3]);
  dBbcGhitRawNode_t* n5 = static_cast<dBbcGhitRawNode_t*>(nl[4]);
  dBbcRawNode_t* n6 = static_cast<dBbcRawNode_t*>(nl[5]);

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

  result = mbbcghitraw_(
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
