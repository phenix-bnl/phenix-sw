/* Automatically generated.  Do not edit. */
#include "mEmcGeaClusterEval2Module.h"
#include "mEmcGeaClusterEval2.h"
#include "PHIODataNode.h"

#include "dEmcEventWrapper.h"

typedef PHIODataNode<dEmcEventWrapper> dEmcEventNode_t;

#include "dEmcGeaTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTrackWrapper> dEmcGeaTrackNode_t;

#include "dEmcGeaTowerTrackWrapper.h"

typedef PHIODataNode<dEmcGeaTowerTrackWrapper> dEmcGeaTowerTrackNode_t;

#include "dEmcClusterExtWrapper.h"

typedef PHIODataNode<dEmcClusterExtWrapper> dEmcClusterExtNode_t;

#include "dEmcGeaTrackClusterWrapper.h"

typedef PHIODataNode<dEmcGeaTrackClusterWrapper> dEmcGeaTrackClusterNode_t;

#include "dEmcGeaClusterTrackWrapper.h"

typedef PHIODataNode<dEmcGeaClusterTrackWrapper> dEmcGeaClusterTrackNode_t;

PHBoolean
mEmcGeaClusterEval2Module::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCEVENT_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCGEATRACK_ST *d2;
  TABLE_HEAD_ST t3;
  DEMCGEATOWERTRACK_ST *d3;
  TABLE_HEAD_ST t4;
  DEMCCLUSTEREXT_ST *d4;
  TABLE_HEAD_ST t5;
  DEMCGEATRACKCLUSTER_ST *d5;
  TABLE_HEAD_ST t6;
  DEMCGEACLUSTERTRACK_ST *d6;

  dEmcEventNode_t* n1 = static_cast<dEmcEventNode_t*>(nl[0]);
  dEmcGeaTrackNode_t* n2 = static_cast<dEmcGeaTrackNode_t*>(nl[1]);
  dEmcGeaTowerTrackNode_t* n3 = static_cast<dEmcGeaTowerTrackNode_t*>(nl[2]);
  dEmcClusterExtNode_t* n4 = static_cast<dEmcClusterExtNode_t*>(nl[3]);
  dEmcGeaTrackClusterNode_t* n5 = static_cast<dEmcGeaTrackClusterNode_t*>(nl[4]);
  dEmcGeaClusterTrackNode_t* n6 = static_cast<dEmcGeaClusterTrackNode_t*>(nl[5]);

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

  result = memcgeaclustereval2_(
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
