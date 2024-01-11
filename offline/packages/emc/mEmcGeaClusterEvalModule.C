#include <Fun4AllReturnCodes.h>

#include <iostream>
#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include "mEmcGeaClusterEvalModule.h"

#include "dEmcEventWrapper.h"

#include "dEmcGeaTrackWrapper.h"

#include "dEmcGeaTowerTrackWrapper.h"

#include "dEmcClusterLocalExtWrapper.h"

#include "dEmcGeaTrackClusterWrapper.h"

#include "dEmcGeaClusterTrackWrapper.h"
typedef PHIODataNode<PHTable> TableNode_t;

#include "phool.h"

using namespace std;

// Default constructor and destructor to pacify CINT
mEmcGeaClusterEvalModule::mEmcGeaClusterEvalModule(): SubsysReco("mEmcGeaClusterEvalModule"){}

mEmcGeaClusterEvalModule::~mEmcGeaClusterEvalModule(){}

int
mEmcGeaClusterEvalModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHCompositeNode *emcNode, *dstNode, *evaNode;

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   cerr << PHWHERE << " Could not find EMC node" << endl;
   return ABORTRUN;
   //   emcNode = new PHCompositeNode("EMC");
   //root->addNode(emcNode);
 }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   cerr << PHWHERE << " Could not find DST node " << endl;
   return ABORTRUN;
   //   dstNode = new PHCompositeNode("DST");
   //   root->addNode(dstNode);
 }

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   cerr << PHWHERE << " Could not find EVA node " << endl;
   return ABORTRUN;
   //   evaNode = new PHCompositeNode("EVA");
   //   root->addNode(evaNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  n = i.findFirst("PHIODataNode", "dEmcEvent");
  if (!n) {
    cerr<< PHWHERE << "ERROR:  'in' parameter dEmcEvent not found" << endl;
    return ABORTRUN;
  }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dEmcGeaTrack");
  if (!n) {
    cerr << PHWHERE << "ERROR:  'in' parameter dEmcGeaTrack not found" << endl;
    // probably was missing a return False...
    return ABORTRUN;
  }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dEmcGeaTowerTrack");
  if (!n) {
    cerr << PHWHERE << "ERROR:  'in' parameter dEmcGeaTowerTrack not found" << endl;
    return ABORTRUN;
  }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dEmcClusterLocalExt");
  if (!n) {
    cerr << PHWHERE << "ERROR:  'in' parameter dEmcClusterLocalExt not found" << endl;
    return ABORTRUN;
  }
  nodes.append(n);

  j = new PHNodeIterator(evaNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTrackCluster")))) {
    cerr << PHWHERE << " Could not find dEmcGeaTrackCluster node" << endl;
    return ABORTRUN;
  }
  delete j;
  nodes.append(d);

  j = new PHNodeIterator(evaNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaClusterTrack")))) {
    cerr << PHWHERE << " Could not find dEmcGeaClusterTrack" << endl;
    return ABORTRUN;
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes) ? EVENT_OK : ABORTRUN;
}
