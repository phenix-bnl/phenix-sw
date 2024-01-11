/****************************************************************************
 *         Convert from mTofPerfect.c to mTofPerfectModule.h
 *                     [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 * File: mTofPerfect.idl
 * Author: Akio Kiyomichi (Univ. of Tsukuba)
 * Description: TOF perfect track reconstruction module
 *              mTofGhitGdigi + mTofGdigiRec
 * Date: 04/09/98 A.Kiyomichi  First Version
 *       05/30/00 A.Kiyomichi  re-create pure PHOOL module
 *****************************************************************************/
#include "mTofPerfectModule.h"
#include "mTofGhitGdigiModule.h"
#include "mTofGdigiRecModule.h"
#include "tofghitWrapper.h"
#include "fkinWrapper.h"
#include "dTofPerfParWrapper.h"
#include "dTofGeoWrapper.h"
#include "dTofGdigiWrapper.h"
#include "dTofGhitGdigiWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dTofGdigiRecWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

PHBoolean
mTofPerfectModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root), *j;
  PHNode *n;
  TableNode_t *d;
  PHTable *w;
  PHCompositeNode *geaNode, *parNode, *tofNode, *dstNode, *outNode;
  
  parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }
  
  tofNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode) {
    tofNode = new PHCompositeNode("TOF");
    root->addNode(tofNode);
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
  
// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = geaNode;
  n = i.findFirst("PHIODataNode", "tofghit");
  if (!n) {
    cout << "ERROR:  'in' parameter tofghit not found" << endl;
     w = new tofghitWrapper("tofghit", 3000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"tofghit");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = geaNode;
  n = i.findFirst("PHIODataNode", "fkin");
  if (!n) {
    cout << "ERROR:  'in' parameter fkin not found" << endl;
     w = new fkinWrapper("fkin", 30000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"fkin");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofPerfPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofPerfPar not found" << endl;
     w = new dTofPerfParWrapper("dTofPerfPar", 1);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofPerfPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dTofGeo");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGeo not found" << endl;
     w = new dTofGeoWrapper("dTofGeo", 1000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGeo");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = tofNode;
  n = i.findFirst("PHIODataNode", "dTofGdigi");
  if (!n) {
    cout << "ERROR:  'inout' parameter dTofGdigi not found" << endl;
     w = new dTofGdigiWrapper("dTofGdigi", 1000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGdigi");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = tofNode;
  n = i.findFirst("PHIODataNode", "dTofGhitGdigi");
  if (!n) {
    cout << "ERROR:  'inout' parameter dTofGhitGdigi not found" << endl;
     w = new dTofGhitGdigiWrapper("dTofGhitGdigi", 3000);
     if (!w) {
       return 1;
     }
     n = new TableNode_t(w,"dTofGhitGdigi");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = dstNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofReconstructed")))) {
     w = new dTofReconstructedWrapper("dTofReconstructed", 300);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofReconstructed");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = tofNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofGdigiRec")))) {
     w = new dTofGdigiRecWrapper("dTofGdigiRec", 300);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dTofGdigiRec");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  //=======================================================================
  // Convert from mTofPerfect.c
  //=======================================================================
  mTofGhitGdigiModule* mTofGhitGdigi = new mTofGhitGdigiModule;
  mTofGdigiRecModule*  mTofGdigiRec  = new mTofGdigiRecModule;

  if(!(mTofGhitGdigi->event(root))) return False;
  if(!(mTofGdigiRec->event(root)))  return False;
  return True;
}
