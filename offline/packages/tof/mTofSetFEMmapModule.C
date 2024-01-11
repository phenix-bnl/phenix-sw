#include "mTofSetFEMmapModule.h"

#include "dTofGeoWrapper.h"

#include "dTofFEMmapWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofSetFEMmapModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofGeo");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGeo not found" << endl;
     w = new dTofGeoWrapper("dTofGeo", 1500);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGeo");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofFEMmap")))) {
     w = new dTofFEMmapWrapper("dTofFEMmap", 960);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofFEMmap");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}
