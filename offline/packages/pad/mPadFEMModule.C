#include "mPadFEMModule.h"

#include "dPadRawWrapper.h"

#include "dPadGhitRawWrapper.h"

#include "dPadGeomWrapper.h"

#include "dPadFEMParWrapper.h"

#include "dPadFEMWrapper.h"

#include "dPadNibbleGhitWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Methods to set and retrieve the pad chamber number
void mPadFEMModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadFEMModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadFEMModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *padNode, *dstNode, *evaNode, *outNode;

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
   cout << "ERROR:  pcnumber is not set up for mPadUnpack\n";
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

  outNode = padNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1FEM")))) {
      w = new dPadFEMWrapper("dPc1FEM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1FEM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2FEM")))) {
      w = new dPadFEMWrapper("dPc2FEM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2FEM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3FEM")))) {
      w = new dPadFEMWrapper("dPc3FEM", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3FEM");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  outNode = evaNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1NibbleGhit")))) {
      w = new dPadNibbleGhitWrapper("dPc1NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1NibbleGhit");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2NibbleGhit")))) {
      w = new dPadNibbleGhitWrapper("dPc2NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2NibbleGhit");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3NibbleGhit")))) {
      w = new dPadNibbleGhitWrapper("dPc3NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3NibbleGhit");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  return callPAM(nodes);
}
