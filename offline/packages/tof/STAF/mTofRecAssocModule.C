#include "mTofRecAssocModule.h"

#include "dTofGeoParWrapper.h"

#include "dTofAssocParWrapper.h"

#include "dTofReconstructedWrapper.h"

#include "dBbcOutWrapper.h"

#include "dCglParticleWrapper.h"

#include "dCglTrackWrapper.h"

#include "dDchTracksWrapper.h"

#include "dPadClusterWrapper.h"

#include "dPadClusterWrapper.h"

#include "dPadClusterWrapper.h"

#include "dTecTrackWrapper.h"

#include "dTofAssociateWrapper.h"
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofRecAssocModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *tofNode, *dstNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 tofNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "TOF"));
 if (!tofNode) {
   tofNode = new PHCompositeNode("TOF");
   root->addNode(tofNode);
 }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofGeoPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGeoPar not found" << endl;
     w = new dTofGeoParWrapper("dTofGeoPar", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGeoPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofAssocPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofAssocPar not found" << endl;
     w = new dTofAssocParWrapper("dTofAssocPar", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofAssocPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dTofReconstructed");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofReconstructed not found" << endl;
     w = new dTofReconstructedWrapper("dTofReconstructed", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofReconstructed");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dBbcOut");
  if (!n) {
    cout << "ERROR:  'in' parameter dBbcOut not found" << endl;
     w = new dBbcOutWrapper("dBbcOut", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dBbcOut");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dCglParticle");
  if (!n) {
    cout << "ERROR:  'in' parameter dCglParticle not found" << endl;
     w = new dCglParticleWrapper("dCglParticle", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dCglParticle");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dCglTrack");
  if (!n) {
    cout << "ERROR:  'in' parameter dCglTrack not found" << endl;
     w = new dCglTrackWrapper("dCglTrack", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dCglTrack");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dDchTracks");
  if (!n) {
    cout << "ERROR:  'in' parameter dDchTracks not found" << endl;
     w = new dDchTracksWrapper("dDchTracks", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dDchTracks");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dPc1Cluster");
  if (!n) {
    cout << "ERROR:  'in' parameter dPc1Cluster not found" << endl;
     w = new dPadClusterWrapper("dPc1Cluster", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPc1Cluster");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dPc2Cluster");
  if (!n) {
    cout << "ERROR:  'in' parameter dPc2Cluster not found" << endl;
     w = new dPadClusterWrapper("dPc2Cluster", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPc2Cluster");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dPc3Cluster");
  if (!n) {
    cout << "ERROR:  'in' parameter dPc3Cluster not found" << endl;
     w = new dPadClusterWrapper("dPc3Cluster", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPc3Cluster");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dTecTrack");
  if (!n) {
    cout << "ERROR:  'in' parameter dTecTrack not found" << endl;
     w = new dTecTrackWrapper("dTecTrack", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTecTrack");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = tofNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofAssociate")))) {
     w = new dTofAssociateWrapper("dTofAssociate", 10);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofAssociate");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
