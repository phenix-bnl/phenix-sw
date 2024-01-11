#include <Fun4AllReturnCodes.h>
#include "mEmcDefGeomModule.h"

#include "dEmcGeaParamsWrapper.h"

#include "dEmcGeometryWrapper.h"


#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

int
mEmcDefGeomModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);
 PHNode *n;
 PHTable *w;
 PHCompositeNode *parNode, *emcNode, *dstNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

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

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaParams");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaParams not found" << endl;
     w = new dEmcGeaParamsWrapper("dEmcGeaParams", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeaParams");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeometry");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeometry not found" << endl;
     w = new dEmcGeometryWrapper("dEmcGeometry", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeometry");
     outNode->addNode(n);
  }
  nodes.append(n);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
