#include <iostream>
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include "mEmcDCMinputModule.h"

#include "dEmcDCMDataWrapper.h"
typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mEmcDCMinputModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;

 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dstNode, *outNode;

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcDCMData")))) {
     w = new dEmcDCMDataWrapper("dEmcDCMData", 500);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcDCMData");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
