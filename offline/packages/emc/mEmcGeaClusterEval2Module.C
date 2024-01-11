#include "mEmcGeaClusterEval2Module.h"

#include "dEmcEventWrapper.h"

#include "dEmcGeaTrackWrapper.h"

#include "dEmcGeaTowerTrackWrapper.h"

#include "dEmcClusterExtWrapper.h"

#include "dEmcGeaTrackClusterWrapper.h"

#include "dEmcGeaClusterTrackWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mEmcGeaClusterEval2Module::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *emcNode, *dstNode, *evaNode, *outNode;

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   emcNode = new PHCompositeNode("EMC");
   root->addNode(emcNode);
 }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = emcNode;
  n = i.findFirst("PHIODataNode", "dEmcEvent");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcEvent not found" << endl;
     w = new dEmcEventWrapper("dEmcEvent", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcEvent");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaTrack");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaTrack not found" << endl;
     w = new dEmcGeaTrackWrapper("dEmcGeaTrack", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcGeaTrack");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaTowerTrack");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaTowerTrack not found" << endl;
     w = new dEmcGeaTowerTrackWrapper("dEmcGeaTowerTrack", 15000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcGeaTowerTrack");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dEmcClusterExt");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcClusterExt not found" << endl;
     w = new dEmcClusterExtWrapper("dEmcClusterExt", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcClusterExt");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTrackCluster")))) {
     w = new dEmcGeaTrackClusterWrapper("dEmcGeaTrackCluster", 10);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcGeaTrackCluster");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaClusterTrack")))) {
     w = new dEmcGeaClusterTrackWrapper("dEmcGeaClusterTrack", 10);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcGeaClusterTrack");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
