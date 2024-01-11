#include "mBbcDCMModule.h"

#include "dBbcFEMWrapper.h"

#include "dBbcDCMWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mBbcDCMModule::event(PHCompositeNode *root) {

  // This is for backward compatibility with calls that assume the default 
  // nodes for input and output

  return event(root,"dBbcFEM","dBbcDCM");
}

PHBoolean 
mBbcDCMModule::event(PHCompositeNode *root,PHString bbcFEMNodeName,PHString bbcDCMNodeName) {

 // bbcFEMNodeName is the name of the input FEM node
 // bbcDCMNodeName is the name of the output DCM node

  //cout << "mBbcDCMModule::event: making " << bbcDCMNodeName << " from " 
  //     << bbcFEMNodeName << endl;

 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dstNode, *outNode;

 dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
 if (!dstNode) {
   dstNode = new PHCompositeNode("DST");
   root->addNode(dstNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "BBC"));
  if (!outNode) {
    outNode = new PHCompositeNode("BBC");
    root->addNode(outNode);
  }

  char *bbcFEMNodeNameChar=bbcFEMNodeName.getString();

  n = i.findFirst("PHIODataNode", bbcFEMNodeName);
  if (!n) {
    cout << PHWHERE << " Input BBC FEM node " << bbcFEMNodeName 
	 << " not found, try to create it!" << endl;
     w = new dBbcFEMWrapper(bbcFEMNodeNameChar, 1);
     if (!w) {
       cout << PHWHERE << " Could not create new FEM node " << bbcFEMNodeName 
	    << " punt!" << endl;
       return 1;
     }
     n = new TableNode_t(w,bbcFEMNodeName);
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
  if (!outNode) {
    outNode = new PHCompositeNode("DCM");
    root->addNode(outNode);
  }

  char *bbcDCMNodeNameChar=bbcDCMNodeName.getString();

  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",bbcDCMNodeName)))) {
    cout << PHWHERE << " Could not find output DCM node " << bbcDCMNodeName 
	 << " try to create it!" << endl;
     w = new dBbcDCMWrapper(bbcDCMNodeNameChar, 1);
     if (!w) {
       cout << PHWHERE << " Could not create new DCM node " << bbcDCMNodeName 
	    << " punt!" << endl;
       return 1;
     }
     d = new TableNode_t(w,bbcDCMNodeName);
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  return callPAM(nodes);
}

