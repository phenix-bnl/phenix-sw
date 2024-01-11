#include <Fun4AllReturnCodes.h>

#include "mEmcCalibTowerModule.h"

#include "dEmcRawDataWrapper.h"

#include "dEmcGeometryWrapper.h"

#include "dEmcEventWrapper.h"

#include "dEmcCalibTowerWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mEmcCalibTowerModule::mEmcCalibTowerModule(): SubsysReco("mEmcCalibTowerModule")
{
}

int
mEmcCalibTowerModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
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

 outNode = emcNode;
  n = i.findFirst("PHIODataNode", "dEmcRawData");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcRawData not found" << endl;
     w = new dEmcRawDataWrapper("dEmcRawData", 25000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcRawData");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeometry");
  if (!n) {
    cout << "ERROR:  'inout' parameter dEmcGeometry not found" << endl;
     w = new dEmcGeometryWrapper("dEmcGeometry", 30000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeometry");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = emcNode;
  n = i.findFirst("PHIODataNode", "dEmcEvent");
  if (!n) {
    cout << "ERROR:  'inout' parameter dEmcEvent not found" << endl;
     w = new dEmcEventWrapper("dEmcEvent", 1);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcEvent");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcCalibTower")))) {
     w = new dEmcCalibTowerWrapper("dEmcCalibTower", 25000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcCalibTower");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
