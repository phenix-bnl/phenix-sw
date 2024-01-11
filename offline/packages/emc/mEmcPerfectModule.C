#include "mEmcPerfectModule.h"

#include "dEmcGeaHitWrapper.h"

#include "dEmcClusterLocalWrapper.h"

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
mEmcPerfectModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *emcNode, *dstNode, *outNode;

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

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = emcNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaHit");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaHit not found" << endl;
     w = new dEmcGeaHitWrapper("dEmcGeaHit", 405000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcGeaHit");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcClusterLocal")))) {
     w = new dEmcClusterLocalWrapper("dEmcClusterLocal", 6000);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcClusterLocal");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = emcNode;
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
