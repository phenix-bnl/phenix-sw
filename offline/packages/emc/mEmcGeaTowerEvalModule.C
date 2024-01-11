#include "mEmcGeaTowerEvalModule.h"

#include "dEmcGeometryWrapper.h"

#include "dEmcEventWrapper.h"

#include "dEmcCalibTowerWrapper.h"

#include "dEmcGeaTrackWrapper.h"

#include "dEmcGeaTowerTrackWrapper.h"

#include "dEmcGeaTowerEvalWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
PHBoolean
mEmcGeaTowerEvalModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *emcNode, *evaNode, *dstNode, *outNode;

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

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeometry");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeometry not found" << endl;
     w = new dEmcGeometryWrapper("dEmcGeometry", 30000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcGeometry");
     outNode->addNode(n);
  }
  nodes.append(n);

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

  outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dEmcCalibTower");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcCalibTower not found" << endl;
     w = new dEmcCalibTowerWrapper("dEmcCalibTower", 10000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dEmcCalibTower");
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

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTowerEval")))) {
     w = new dEmcGeaTowerEvalWrapper("dEmcGeaTowerEval", 10);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dEmcGeaTowerEval");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
