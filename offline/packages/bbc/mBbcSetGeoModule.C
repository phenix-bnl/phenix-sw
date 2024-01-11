#include "mBbcSetGeoModule.h"
#include "dBbcGeoWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcSetGeoModule::event(PHCompositeNode *root) {
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

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!outNode) {
    outNode = new PHCompositeNode("PAR");
    root->addNode(outNode);
  }

  n = i.findFirst("PHIODataNode", "dBbcGeo");
  if (!n) {
    cout << "ERROR:  'inout' parameter dBbcGeo not found" << endl;
     w = new dBbcGeoWrapper("dBbcGeo", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dBbcGeo");
     outNode->addNode(n);
  }
  nodes.append(n);

  return callPAM(nodes);
}
