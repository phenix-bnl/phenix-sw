#include "mPadEvaluateModule.h"

#include "dPadEvalParWrapper.h"

#include "dPadGeomWrapper.h"

#include "pcghitWrapper.h"

#include "dPadRawWrapper.h"

#include "dPadClusterWrapper.h"

#include "dPadGhitRawWrapper.h"

#include "dPadRawClusWrapper.h"

#include "dPadGhitClusWrapper.h"

#include "dPadEvalWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mPadEvaluateModule::mPadEvaluateModule(){
  pcnumber = -1;
}

// Methods to set and retrieve the pad chamber number
void mPadEvaluateModule::set_pcnumber(int ipc) {pcnumber = ipc;}
int mPadEvaluateModule::get_pcnumber() {return pcnumber;}

PHBoolean
mPadEvaluateModule::event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *parNode, *padNode, *geaNode, *dstNode, *evaNode, *outNode;

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
   cout << "ERROR:  pcnumber is not set up for mPadEvaluate\n";
   return 1;
 }

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dPadEvalPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dPadEvalPar not found" << endl;
     w = new dPadEvalParWrapper("dPadEvalPar", 10);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dPadEvalPar");
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
    n = i.findFirst("PHIODataNode", "dPc1Cluster");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1Cluster not found" << endl;
      w = new dPadClusterWrapper("dPc1Cluster", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1Cluster");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2Cluster");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2Cluster not found" << endl;
      w = new dPadClusterWrapper("dPc2Cluster", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2Cluster");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3Cluster");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3Cluster not found" << endl;
      w = new dPadClusterWrapper("dPc3Cluster", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3Cluster");
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

  outNode = padNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1RawClus");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc1RawClus not found" << endl;
      w = new dPadRawClusWrapper("dPc1RawClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1RawClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2RawClus");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc2RawClus not found" << endl;
      w = new dPadRawClusWrapper("dPc2RawClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2RawClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3RawClus");
    if (!n) {
      cout << "ERROR:  'in' parameter dPc3RawClus not found" << endl;
      w = new dPadRawClusWrapper("dPc3RawClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3RawClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

  outNode = evaNode;
  if (pcnumber == 0) {
    n = i.findFirst("PHIODataNode", "dPc1GhitClus");
    if (!n) {
      cout << "ERROR:  'inout' parameter dPc1GhitClus not found" << endl;
      w = new dPadGhitClusWrapper("dPc1GhitClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc1GhitClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 1) {
    n = i.findFirst("PHIODataNode", "dPc2GhitClus");
    if (!n) {
      cout << "ERROR:  'inout' parameter dPc2GhitClus not found" << endl;
      w = new dPadGhitClusWrapper("dPc2GhitClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc2GhitClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }
  if (pcnumber == 2) {
    n = i.findFirst("PHIODataNode", "dPc3GhitClus");
    if (!n) {
      cout << "ERROR:  'inout' parameter dPc3GhitClus not found" << endl;
      w = new dPadGhitClusWrapper("dPc3GhitClus", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dPc3GhitClus");
      outNode->addNode(n);
    }
    nodes.append(n);
  }

  outNode = padNode;
  if (pcnumber == 0) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc1Eval")))) {
      w = new dPadEvalWrapper("dPc1Eval", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc1Eval");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 1) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc2Eval")))) {
      w = new dPadEvalWrapper("dPc2Eval", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc2Eval");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }
  if (pcnumber == 2) {
    j = new PHNodeIterator(outNode);
    if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dPc3Eval")))) {
      w = new dPadEvalWrapper("dPc3Eval", 10);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dPc3Eval");
      outNode->addNode(d);
    }
    delete j;
    nodes.append(d);
  }

  return callPAM(nodes);
}
