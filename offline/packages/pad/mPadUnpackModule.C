#include "mPadUnpackModule.h"

#include "dPadDCMWrapper.h"

#include "dPadGeomWrapper.h"

#include "dPadNibbleGhitWrapper.h"

#include "dPadFEMParWrapper.h"

#include "dPadRawWrapper.h"

#include "dPadGhitRawWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mPadUnpackModule::mPadUnpackModule(){
  pcnumber = -1;
}

// Methods to set and retrieve the pad chamber number
void mPadUnpackModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadUnpackModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadUnpackModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dcmNode, *parNode, *padNode, *dstNode, *evaNode, *outNode;

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

  if (pcnumber<0 || pcnumber>2) {
    cout << "ERROR:  pcnumber is not set up for mPadUnpack\n";
    return 1;
  }

  outNode = dcmNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1DCM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1DCM not found" << endl;
      w = new dPadDCMWrapper("dPc1DCM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1DCM");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2DCM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2DCM not found" << endl;
      w = new dPadDCMWrapper("dPc2DCM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2DCM");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3DCM");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3DCM not found" << endl;
      w = new dPadDCMWrapper("dPc3DCM", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3DCM");
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

  outNode = evaNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1NibbleGhit");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1NibbleGhit not found" << endl;
      w = new dPadNibbleGhitWrapper("dPc1NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1NibbleGhit");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2NibbleGhit");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2NibbleGhit not found" << endl;
      w = new dPadNibbleGhitWrapper("dPc2NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2NibbleGhit");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3NibbleGhit");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3NibbleGhit not found" << endl;
      w = new dPadNibbleGhitWrapper("dPc3NibbleGhit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3NibbleGhit");
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

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (pcnumber == 0) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1Raw")))) {
      w = new dPadRawWrapper("dPc1Raw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1Raw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2Raw")))) {
      w = new dPadRawWrapper("dPc2Raw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2Raw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3Raw")))) {
      w = new dPadRawWrapper("dPc3Raw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3Raw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (pcnumber == 0) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1GhitRaw")))) {
      w = new dPadGhitRawWrapper("dPc1GhitRaw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1GhitRaw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2GhitRaw")))) {
      w = new dPadGhitRawWrapper("dPc2GhitRaw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2GhitRaw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3GhitRaw")))) {
      w = new dPadGhitRawWrapper("dPc3GhitRaw", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3GhitRaw");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  return callPAM(nodes);
}
