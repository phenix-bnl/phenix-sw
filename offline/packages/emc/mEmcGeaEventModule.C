#include <Fun4AllReturnCodes.h>
#include "mEmcGeaEventModule.h"

#include "headerWrapper.h"

#include "dEmcEventWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mEmcGeaEventModule::mEmcGeaEventModule(): SubsysReco("mEmcGeaEventModule")
{
}

int
mEmcGeaEventModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *geaNode, *emcNode, *outNode;

 geaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "GEA"));
 if (!geaNode) {
   geaNode = new PHCompositeNode("GEA");
   root->addNode(geaNode);
 }

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   emcNode = new PHCompositeNode("EMC");
   root->addNode(emcNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = geaNode;
  n = i.findFirst("PHIODataNode", "header");
  if (!n) {
    cout << "ERROR:  'in' parameter header not found" << endl;
     w = new headerWrapper("header", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???;
     }
     n = new TableNode_t(w,"header");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = emcNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcEvent")))) {
     w = new dEmcEventWrapper("dEmcEvent", 1);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcEvent");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
