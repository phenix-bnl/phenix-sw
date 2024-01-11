#include "mTofAssocPidModule.h"
#include "dTofPidParWrapper.h"
#include "dTofAssociateWrapper.h"
#include "dTofPidWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofAssocPidModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *tofNode, *dstNode, *outNode;

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

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

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofPidPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofPidPar not found" << endl;
     w = new dTofPidParWrapper("dTofPidPar", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofPidPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = tofNode;
  n = i.findFirst("PHIODataNode", "dTofAssociate");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofAssociate not found" << endl;
     w = new dTofAssociateWrapper("dTofAssociate", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofAssociate");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofPid")))) {
     w = new dTofPidWrapper("dTofPid", 10);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofPid");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
