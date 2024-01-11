#include "mBbcFEMModule.h"
#include "dBbcRawWrapper.h"
#include "dBbcFEMWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcFEMModule::event(PHCompositeNode *root) {
  
  return event(root,"dBbcRaw","dBbcFEM");

}


PHBoolean
mBbcFEMModule::event(PHCompositeNode *root,PHString dBbcRawNodeName,PHString dBbcFEMNodeName) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dstNode, *outNode;

 //cout << "mBbcFEMModule::event: making " << dBbcFEMNodeName << " from " 
 //     << dBbcRawNodeName << endl;

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
  if (!outNode) {
    outNode = new PHCompositeNode("DST");
    root->addNode(outNode);
  }

  char *dBbcRawNodeNameChar = dBbcRawNodeName.getString();

  n = i.findFirst("PHIODataNode", dBbcRawNodeName);
  if (!n) {
    cout << PHWHERE << " ERROR: input BBC raw node " << dBbcRawNodeName 
	 << " not found, try to create it!" << endl;
     w = new dBbcRawWrapper(dBbcRawNodeNameChar, 128);
     if (!w) {
       cout << PHWHERE << " Could not create new BBC raw node " 
	    << dBbcRawNodeNameChar << " punt!" << endl;
       return 1;
     }
     n = new TableNode_t(w,dBbcRawNodeName);
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "BBC"));
  if (!outNode) {
    outNode = new PHCompositeNode("BBC");
    root->addNode(outNode);
  }

  char *dBbcFEMNodeNameChar = dBbcFEMNodeName.getString();

  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",dBbcFEMNodeName)))) {
    cout << PHWHERE << " Did not find output BBC FEM node " << dBbcFEMNodeName 
	 << " try to create it!" << endl;
     w = new dBbcFEMWrapper(dBbcFEMNodeNameChar, 1);
     if (!w) {
       cout << PHWHERE << " Could not create " << dBbcFEMNodeNameChar 
	    << " exit!" << endl;
       return 1;
     }
     d = new TableNode_t(w,dBbcFEMNodeName);
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}


