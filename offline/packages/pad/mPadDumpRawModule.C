#include <mPadDumpRawModule.h>

#include <dPadRawWrapper.h>

#include <dPadGhitRawWrapper.h>

#include <dPadFEMParWrapper.h>

#include <PHNode.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTable.hh>

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mPadDumpRawModule::mPadDumpRawModule(){
  pcnumber = -1;
}

// Methods to set and retrieve the pad chamber number
void mPadDumpRawModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadDumpRawModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadDumpRawModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);
 PHNode *n;
 PHTable *w;
 PHCompositeNode *padNode, *parNode, *evaNode, *dstNode, *outNode;

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

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 // Get out if the user hasn't selected a pad chamber number
 if (pcnumber<0 || pcnumber>2) {
   cout << "ERROR:  pcnumber is not set up for mPadDumpRaw\n";
   return 1;
 }

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

  outNode = evaNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1GhitRaw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1GhitRaw not found" << endl;
      w = new dPadGhitRawWrapper("dPc1GhitRaw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1GhitRaw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2GhitRaw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2GhitRaw not found" << endl;
      w = new dPadGhitRawWrapper("dPc2GhitRaw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2GhitRaw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3GhitRaw");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3GhitRaw not found" << endl;
      w = new dPadGhitRawWrapper("dPc3GhitRaw", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3GhitRaw");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadFEMPar");
  if (!n) {
    cout << "ERROR:  'inout' parameter dPadFEMPar not found" << endl;
     w = new dPadFEMParWrapper("dPadFEMPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadFEMPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  return callPAM(nodes);
}
