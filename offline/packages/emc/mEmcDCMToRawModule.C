#include <Fun4AllReturnCodes.h>
#include "mEmcDCMToRawModule.h"

#include "dEmcDCMDataWrapper.h"

#include "dEmcRawDataWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

int
mEmcDCMToRawModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *emcNode, *dcmNode, *outNode;

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
 }

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   emcNode = new PHCompositeNode("EMC");
   root->addNode(emcNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = dcmNode;
  n = i.findFirst("PHIODataNode", "dEmcDCMData");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcDCMData not found" << endl;
     w = new dEmcDCMDataWrapper("dEmcDCMData", 500);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcDCMData");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = emcNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcRawData")))) {
     w = new dEmcRawDataWrapper("dEmcRawData", 15000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcRawData");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
