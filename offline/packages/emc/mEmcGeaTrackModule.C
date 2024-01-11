#include <Fun4AllReturnCodes.h>
#include "mEmcGeaTrackModule.h"

#include "dEmcGeaTrackTowerWrapper.h"

#include "dEmcGeaTrackWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mEmcGeaTrackModule::mEmcGeaTrackModule(): SubsysReco("mEmcGeaTrackModule")
{
}

int
mEmcGeaTrackModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *evaNode, *outNode;

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = evaNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaTrackTower");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaTrackTower not found" << endl;
     w = new dEmcGeaTrackTowerWrapper("dEmcGeaTrackTower", 7500);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeaTrackTower");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTrack")))) {
     w = new dEmcGeaTrackWrapper("dEmcGeaTrack", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcGeaTrack");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
