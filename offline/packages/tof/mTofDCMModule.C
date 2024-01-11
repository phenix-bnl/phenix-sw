#include "mTofDCMModule.h"
#include "dTofFEMWrapper.h"
#include "dTofDCMParWrapper.h"
#include "dTofDCMWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofDCMModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *tofNode, *dcmNode, *outNode;

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

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = tofNode;
  n = i.findFirst("PHIODataNode", "dTofFEM");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofFEM not found" << endl;
     w = new dTofFEMWrapper("dTofFEM", 8);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofFEM");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofDCMPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofDCMPar not found" << endl;
     w = new dTofDCMParWrapper("dTofDCMPar", 8);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofDCMPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dcmNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofDCM")))) {
     w = new dTofDCMWrapper("dTofDCM", 8);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofDCM");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
