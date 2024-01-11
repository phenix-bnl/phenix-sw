#include "mDchDCMModule.h"

#include "dDchFEMWrapper.h"

#include "dDchDCMParWrapper.h"

#include "dDchDCMWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mDchDCMModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *dchNode, *dcmNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 dchNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCH"));
 if (!dchNode) {
   dchNode = new PHCompositeNode("DCH");
   root->addNode(dchNode);
 }

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = dchNode;
  n = i.findFirst("PHIODataNode", "dDchFEM");
  if (!n) {
    cout << "ERROR:  'in' parameter dDchFEM not found" << endl;
     w = new dDchFEMWrapper("dDchFEM", 160);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dDchFEM");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dDchDCMPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dDchDCMPar not found" << endl;
     w = new dDchDCMParWrapper("dDchDCMPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dDchDCMPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dcmNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchDCM")))) {
     w = new dDchDCMWrapper("dDchDCM", 160);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dDchDCM");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
