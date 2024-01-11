#include "mTofFEMModule.h"

#include "dTofRawWrapper.h"

#include "dTofGhitRawWrapper.h"

#include "dTofFEMmapWrapper.h"

#include "dTofFEMWrapper.h"

#include "dTofFEMhitGhitWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofFEMModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *tofNode, *dstNode, *evaNode, *outNode;

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

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 outNode = dstNode;
  n = i.findFirst("PHIODataNode", "dTofRaw");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofRaw not found" << endl;
     w = new dTofRawWrapper("dTofRaw", 150);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofRaw");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  n = i.findFirst("PHIODataNode", "dTofGhitRaw");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGhitRaw not found" << endl;
     w = new dTofGhitRawWrapper("dTofGhitRaw", 3000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGhitRaw");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofFEMmap");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofFEMmap not found" << endl;
     w = new dTofFEMmapWrapper("dTofFEMmap", 960);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofFEMmap");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = tofNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofFEM")))) {
     w = new dTofFEMWrapper("dTofFEM", 8);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofFEM");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofFEMhitGhit")))) {
     w = new dTofFEMhitGhitWrapper("dTofFEMhitGhit", 3000);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofFEMhitGhit");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
