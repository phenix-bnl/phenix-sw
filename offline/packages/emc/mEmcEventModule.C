#include "mEmcEventModule.h"

#include "dBbcOutWrapper.h"

#include "dEmcEventWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mEmcEventModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dstNode, *emcNode, *outNode;

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

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dBbcOut");
  if (!n) {
    cout << "ERROR:  'in' parameter dBbcOut not found" << endl;
     w = new dBbcOutWrapper("dBbcOut", 3);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dBbcOut");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = emcNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcEvent")))) {
     w = new dEmcEventWrapper("dEmcEvent", 1);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcEvent");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
