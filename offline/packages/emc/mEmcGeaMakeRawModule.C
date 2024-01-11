#include <Fun4AllReturnCodes.h>
#include <mEmcGeaMakeRawModule.h>

#include <headerWrapper.h>

#include <dEmcGeaHitWrapper.h>

#include <dEmcGeaParamsWrapper.h>

#include <dEmcRespParWrapper.h>

#include <dEmcGeometryWrapper.h>

#include <dEmcGeaTrackTowerWrapper.h>

#include <dEmcGeaTowerTrackWrapper.h>

#include <dEmcRawDataWrapper.h>

#include <PHNode.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTable.hh>

#include <recoConsts.h>

#include <cassert>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// Default constructor and destructor to pacify CINT
mEmcGeaMakeRawModule::mEmcGeaMakeRawModule(): SubsysReco("mEmcGeaMakeRawModule")
{
  firstevent = true;
}

int
mEmcGeaMakeRawModule::process_event(PHCompositeNode *root) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root), *j;
 PHNode *n;
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *geaNode, *emcNode, *parNode, *evaNode, *outNode;

 parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
 if (!parNode) {
   parNode = new PHCompositeNode("PAR");
   root->addNode(parNode);
 }

 emcNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EMC"));
 if (!emcNode) {
   emcNode = new PHCompositeNode("EMC");
   root->addNode(emcNode);
 }

 geaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "GEA"));
 if (!geaNode) {
   geaNode = new PHCompositeNode("GEA");
   root->addNode(geaNode);
 }

 evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
 if (!evaNode) {
   evaNode = new PHCompositeNode("EVA");
   root->addNode(evaNode);
 }


// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  outNode = geaNode;
  n = i.findFirst("PHIODataNode", "header");
  if (!n) {
    cout << "ERROR:  'in' parameter header not found" << endl;
     w = new headerWrapper("header", 10);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"header");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = emcNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaHit");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaHit not found" << endl;
     w = new dEmcGeaHitWrapper("dEmcGeaHit", 405000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeaHit");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeaParams");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeaParams not found" << endl;
     w = new dEmcGeaParamsWrapper("dEmcGeaParams", 400);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeaParams");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcRespPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcRespPar not found" << endl;
     w = new dEmcRespParWrapper("dEmcRespPar", 1);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcRespPar");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = parNode;
  n = i.findFirst("PHIODataNode", "dEmcGeometry");
  if (!n) {
    cout << "ERROR:  'in' parameter dEmcGeometry not found" << endl;
     w = new dEmcGeometryWrapper("dEmcGeometry", 30000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     n = new TableNode_t(w,"dEmcGeometry");
     outNode->addNode(n);
  }
  nodes.append(n);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTrackTower")))) {
     w = new dEmcGeaTrackTowerWrapper("dEmcGeaTrackTower", 7500);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcGeaTrackTower");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = evaNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaTowerTrack")))) {
     w = new dEmcGeaTowerTrackWrapper("dEmcGeaTowerTrack", 15000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcGeaTowerTrack");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);

  outNode = emcNode;
  j = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcRawData")))) {
     w = new dEmcRawDataWrapper("dEmcRawData", 15000);
     if (!w) {
       return ABORTRUN; // was 1 = true = OK, why ???
     }
     d = new TableNode_t(w,"dEmcRawData");
     outNode->addNode(d);
  }
  delete j;
  nodes.append(d);


  if( firstevent ){
    recoConsts *rc = recoConsts::instance();
    assert( rc != 0 );
    lowgain_convfac = rc->get_FloatFlag("EMCTOWERLOWGAIN", 0.001);
    highgain_convfac = rc->get_FloatFlag("EMCTOWERHIGHGAIN", 0.008);
    
    cout << __PRETTY_FUNCTION__ << ":   "
	 << "lowgain = " << lowgain_convfac << "   highgain = " << highgain_convfac
	 << endl;
    
    firstevent = false;
  }

  
  return callPAM(lowgain_convfac, highgain_convfac, nodes) ? EVENT_OK : ABORTRUN;
}
