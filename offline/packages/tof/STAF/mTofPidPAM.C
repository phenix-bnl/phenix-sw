/* Automatically generated.  Do not edit. */
#include "mTofPidModule.h"
#include "mTofPid.h"
#include "PHIODataNode.h"

#include "dTofGeoParWrapper.h"

typedef PHIODataNode<dTofGeoParWrapper> dTofGeoParNode_t;

#include "dTofAssocParWrapper.h"

typedef PHIODataNode<dTofAssocParWrapper> dTofAssocParNode_t;

#include "dTofPidParWrapper.h"

typedef PHIODataNode<dTofPidParWrapper> dTofPidParNode_t;

#include "dTofReconstructedWrapper.h"

typedef PHIODataNode<dTofReconstructedWrapper> dTofReconstructedNode_t;

#include "dBbcOutWrapper.h"

typedef PHIODataNode<dBbcOutWrapper> dBbcOutNode_t;

#include "dCglParticleWrapper.h"

typedef PHIODataNode<dCglParticleWrapper> dCglParticleNode_t;

#include "dCglTrackWrapper.h"

typedef PHIODataNode<dCglTrackWrapper> dCglTrackNode_t;

#include "dDchTracksWrapper.h"

typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dPadClusterWrapper.h"

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

#include "dTecTrackWrapper.h"

typedef PHIODataNode<dTecTrackWrapper> dTecTrackNode_t;

#include "dTofPidWrapper.h"

typedef PHIODataNode<dTofPidWrapper> dTofPidNode_t;

PHBoolean
mTofPidModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DTOFGEOPAR_ST *d1;
  TABLE_HEAD_ST t2;
  DTOFASSOCPAR_ST *d2;
  TABLE_HEAD_ST t3;
  DTOFPIDPAR_ST *d3;
  TABLE_HEAD_ST t4;
  DTOFRECONSTRUCTED_ST *d4;
  TABLE_HEAD_ST t5;
  DBBCOUT_ST *d5;
  TABLE_HEAD_ST t6;
  DCGLPARTICLE_ST *d6;
  TABLE_HEAD_ST t7;
  DCGLTRACK_ST *d7;
  TABLE_HEAD_ST t8;
  DDCHTRACKS_ST *d8;
  TABLE_HEAD_ST t9;
  DPADCLUSTER_ST *d9;
  TABLE_HEAD_ST t10;
  DPADCLUSTER_ST *d10;
  TABLE_HEAD_ST t11;
  DPADCLUSTER_ST *d11;
  TABLE_HEAD_ST t12;
  DTECTRACK_ST *d12;
  TABLE_HEAD_ST t13;
  DTOFPID_ST *d13;

  dTofGeoParNode_t* n1 = static_cast<dTofGeoParNode_t*>(nl[0]);
  dTofAssocParNode_t* n2 = static_cast<dTofAssocParNode_t*>(nl[1]);
  dTofPidParNode_t* n3 = static_cast<dTofPidParNode_t*>(nl[2]);
  dTofReconstructedNode_t* n4 = static_cast<dTofReconstructedNode_t*>(nl[3]);
  dBbcOutNode_t* n5 = static_cast<dBbcOutNode_t*>(nl[4]);
  dCglParticleNode_t* n6 = static_cast<dCglParticleNode_t*>(nl[5]);
  dCglTrackNode_t* n7 = static_cast<dCglTrackNode_t*>(nl[6]);
  dDchTracksNode_t* n8 = static_cast<dDchTracksNode_t*>(nl[7]);
  dPadClusterNode_t* n9 = static_cast<dPadClusterNode_t*>(nl[8]);
  dPadClusterNode_t* n10 = static_cast<dPadClusterNode_t*>(nl[9]);
  dPadClusterNode_t* n11 = static_cast<dPadClusterNode_t*>(nl[10]);
  dTecTrackNode_t* n12 = static_cast<dTecTrackNode_t*>(nl[11]);
  dTofPidNode_t* n13 = static_cast<dTofPidNode_t*>(nl[12]);

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
  t9 = n9->getData()->TableHeader();
  d9 = n9->getData()->TableData();
  t10 = n10->getData()->TableHeader();
  d10 = n10->getData()->TableData();
  t11 = n11->getData()->TableHeader();
  d11 = n11->getData()->TableData();
  t12 = n12->getData()->TableHeader();
  d12 = n12->getData()->TableData();
  t13 = n13->getData()->TableHeader();
  d13 = n13->getData()->TableData();

  result = mtofpid_(
    &t1, d1,
    &t2, d2,
    &t3, d3,
    &t4, d4,
    &t5, d5,
    &t6, d6,
    &t7, d7,
    &t8, d8,
    &t9, d9,
    &t10, d10,
    &t11, d11,
    &t12, d12,
    &t13, d13                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);
  n3->getData()->SetRowCount(t3.nok);
  n4->getData()->SetRowCount(t4.nok);
  n5->getData()->SetRowCount(t5.nok);
  n6->getData()->SetRowCount(t6.nok);
  n7->getData()->SetRowCount(t7.nok);
  n8->getData()->SetRowCount(t8.nok);
  n9->getData()->SetRowCount(t9.nok);
  n10->getData()->SetRowCount(t10.nok);
  n11->getData()->SetRowCount(t11.nok);
  n12->getData()->SetRowCount(t12.nok);
  n13->getData()->SetRowCount(t13.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
