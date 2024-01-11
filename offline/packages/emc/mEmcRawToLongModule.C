#include <Fun4AllReturnCodes.h>
#include "mEmcRawToLongModule.h"

#include "dEmcRawDataWrapper.h"

#include "dEmcDCMLongDataWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

int
mEmcRawToLongModule::process_event(PHCompositeNode *root) {

  // This is for backward compatibility - selects the default in and out nodes

  return process_event(root,"dEmcRawData","dEmcDCMLongData");
}

int
mEmcRawToLongModule::process_event(PHCompositeNode *root,PHString dEmcRawNodeName,PHString dEmcDCMLongNodeName) {

 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dcmNode, *emcNode, *outNode;

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   emcNode = new PHCompositeNode("EMC");
   root->addNode(emcNode);
 }

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 outNode = emcNode;
  n = i.findFirst("PHIODataNode", dEmcRawNodeName);
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcRawData not found" << endl;
    char *dEmcRawNodeNameChar = dEmcRawNodeName.getString();
     w = new dEmcRawDataWrapper(dEmcRawNodeNameChar, 15000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,dEmcRawNodeName);
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dcmNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",dEmcDCMLongNodeName)))) {
    char *dEmcDCMLongNodeNameChar = dEmcDCMLongNodeName.getString();
     w = new dEmcDCMLongDataWrapper(dEmcDCMLongNodeNameChar, 500);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,dEmcDCMLongNodeName);

     w->Print((Option_t*)0);

     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
