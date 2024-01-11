#include <Fun4AllReturnCodes.h>
#include "mEmcGeaParamsModule.h"

#include "emcparWrapper.h"

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

// Default constructor and destructor to pacify CINT
mEmcGeaParamsModule::mEmcGeaParamsModule(): SubsysReco("mEmcGeaParamsModule")
{
  wascalled = false;
}

int
mEmcGeaParamsModule::process_event(PHCompositeNode *root) {

  if( wascalled ) return EVENT_OK;
  wascalled = true;

 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *geaNode, *parNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 geaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "GEA"));
 if (!geaNode) {
   geaNode = new PHCompositeNode("GEA");
   root->addNode(geaNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = geaNode;
  n = i.findFirst("PHIODataNode", "emcpar");
  if (!n) {
    cout << "ERROR:  'in' parameter emcpar not found" << endl;
     w = new emcparWrapper("emcpar", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"emcpar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaParams")))) {
     w = new dEmcGeaParamsWrapper("dEmcGeaParams", 400);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcGeaParams");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeometry")))) {
     w = new dEmcGeometryWrapper("dEmcGeometry", 30000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcGeometry");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
