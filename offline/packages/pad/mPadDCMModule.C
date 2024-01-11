#include "mPadDCMModule.h"

#include "dPadFEMWrapper.h"

#include "dPadDCMParWrapper.h"

#include "dPadDCMWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mPadDCMModule::mPadDCMModule(){
  pcnumber = -1;
}

// Methods to set and retrieve the pad chamber number
void mPadDCMModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadDCMModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadDCMModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *padNode, *dcmNode, *outNode;

 dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
 if (!dcmNode) {
   dcmNode = new PHCompositeNode("DCM");
   root->addNode(dcmNode);
 }

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

 // Get out if the user hasn't selected a pad chamber number
 if (pcnumber<0 || pcnumber>2) {
   cout << "ERROR:  pcnumber is not set up for mPadDCM\n";
   return 1;
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = padNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1FEM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1FEM not found" << endl;
      w = new dPadFEMWrapper("dPc1FEM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1FEM");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2FEM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2FEM not found" << endl;
      w = new dPadFEMWrapper("dPc2FEM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2FEM");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3FEM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3FEM not found" << endl;
      w = new dPadFEMWrapper("dPc3FEM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3FEM");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadDCMPar");
  if (!n) {
    cout << "ERROR:  'inout' parameter dPadDCMPar not found" << endl;
     w = new dPadDCMParWrapper("dPadDCMPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadDCMPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dcmNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1DCM")))) {
      w = new dPadDCMWrapper("dPc1DCM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1DCM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2DCM")))) {
      w = new dPadDCMWrapper("dPc2DCM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2DCM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3DCM")))) {
      w = new dPadDCMWrapper("dPc3DCM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3DCM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  return callPAM(nodes);
}
