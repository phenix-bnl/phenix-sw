#include "mPadRecModule.h"

#include "dPadRecParWrapper.h"

#include "dPadGeomWrapper.h"

#include "dPadRawWrapper.h"

#include "dPadClusterWrapper.h"

#include "dPadRawClusWrapper.h"

#include "dPad23ParWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mPadRecModule::mPadRecModule(){
  pcnumber = -1;
}
// Methods to set and retrieve the pad chamber number
void mPadRecModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadRecModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadRecModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *padNode, *dstNode, *outNode;

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

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 // Get out if the user hasn't selected a pad chamber number
 if (pcnumber<0 || pcnumber>2) {
   cout << "ERROR:  pcnumber is not set up for mPadUnpack\n";
   return 1;
 }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadRecPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dPadRecPar not found" << endl;
     w = new dPadRecParWrapper("dPadRecPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadRecPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadGeom");
  if (!n) {
    cout << "ERROR:  'in' parameter dPadGeom not found" << endl;
     w = new dPadGeomWrapper("dPadGeom", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadGeom");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1Raw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1Raw not found" << endl;
      w = new dPadRawWrapper("dPc1Raw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1Raw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2Raw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2Raw not found" << endl;
      w = new dPadRawWrapper("dPc2Raw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2Raw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3Raw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3Raw not found" << endl;
      w = new dPadRawWrapper("dPc3Raw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3Raw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

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

  outNode = padNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1RawClus")))) {
      w = new dPadRawClusWrapper("dPc1RawClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1RawClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2RawClus")))) {
      w = new dPadRawClusWrapper("dPc2RawClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2RawClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3RawClus")))) {
      w = new dPadRawClusWrapper("dPc3RawClus", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3RawClus");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPad23Par");
  if (!n) {
    cout << "ERROR:  'in' parameter dPad23Par not found" << endl;
    w = new dPad23ParWrapper("dPad23Par", 10);
    if (!w) {
      return 1;
    }
    n = new TableNode_t(w,"dPad23Par");
    outNode->addNode(n);
  }
  nodes.append(n);

  return callPAM(nodes);
}
