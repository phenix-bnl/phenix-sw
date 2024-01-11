#include "mPadFastSimModule.h"

#include "pcghitWrapper.h"

#include "dPadFastSimParWrapper.h"

#include "dPadClusterWrapper.h"

#include "dPadGhitClusWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Methods to set and retrieve the pad chamber number
void mPadFastSimModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadFastSimModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadFastSimModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *geaNode, *parNode, *padNode, *dstNode, *evaNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 padNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAD"));
 if (!padNode) {
   padNode = new PHCompositeNode("PAD");
   root->addNode(padNode);
 }

 geaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "GEA"));
 if (!geaNode) {
   geaNode = new PHCompositeNode("GEA");
   root->addNode(geaNode);
 }

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 // Get out if the user hasn't selected a pad chamber number
 if (pcnumber<0 || pcnumber>2) {
   cout << "ERROR:  pcnumber is not set up for mPadFastSim\n";
   return 1;
 }

  outNode = geaNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "pc1ghit");
    if (!n) {
      cout << "ERROR:  'in' parameter pc1ghit not found" << endl;
      w = new pcghitWrapper("pc1ghit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"pc1ghit");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "pc2ghit");
    if (!n) {
      cout << "ERROR:  'in' parameter pc2ghit not found" << endl;
      w = new pcghitWrapper("pc2ghit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"pc2ghit");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "pc3ghit");
    if (!n) {
      cout << "ERROR:  'in' parameter pc3ghit not found" << endl;
      w = new pcghitWrapper("pc3ghit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"pc3ghit");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadFastSimPar");
  if (!n) {
    cout << "ERROR:  'inout' parameter dPadFastSimPar not found" << endl;
     w = new dPadFastSimParWrapper("dPadFastSimPar", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadFastSimPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1Cluster")))) {
      w = new dPadClusterWrapper("dPc1Cluster", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1Cluster");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2Cluster")))) {
      w = new dPadClusterWrapper("dPc2Cluster", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2Cluster");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3Cluster")))) {
      w = new dPadClusterWrapper("dPc3Cluster", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3Cluster");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  outNode = evaNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1GhitClus")))) {
      w = new dPadGhitClusWrapper("dPc1GhitClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1GhitClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2GhitClus")))) {
      w = new dPadGhitClusWrapper("dPc2GhitClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2GhitClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3GhitClus")))) {
      w = new dPadGhitClusWrapper("dPc3GhitClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3GhitClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  return callPAM(nodes);
}
