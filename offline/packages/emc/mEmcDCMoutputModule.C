#include "mEmcDCMoutputModule.h"

#include "dEmcDCMDataWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mEmcDCMoutputModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);
 PHNode *n;
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
  n = i.findFirst("PHIODataNode", "dEmcDCMData");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcDCMData not found" << endl;
     w = new dEmcDCMDataWrapper("dEmcDCMData", 500);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcDCMData");
     outNode->addNode(n);
  }
  nodes.append(n);

  return callPAM(nodes);
}
