#include "VtxMixer.hh"
#include "BbcOut.h"
#include "headerWrapper.h"
#include "dEmcEventWrapper.h"
#include "getClass.h"
#include "Fun4AllReturnCodes.h"
#include "VtxOut.h"

#include <iostream>

using namespace std;
typedef PHIODataNode<PHObject> PHObjectNode_t;

int VtxMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}
// merge Vtx hits (Sasha Lebedev 24-09-2002 lebedev@iastate.edu)
int VtxMixer::merge(){
  if((!node1)||(!node2)||(!node3)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  PHCompositeNode*embedNode = static_cast<PHCompositeNode*>(iter3.findFirst("PHCompositeNode","EMBED"));
  if(!embedNode){
    embedNode = new PHCompositeNode("EMBED");
    node3->addNode(embedNode);
  }
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter3.findFirst("PHCompositeNode", "DST"));
  if (!dstNode){
  }

  VtxOut *vtxout2 = findNode::getClass<VtxOut>(node2, "VtxOut");
  if (!vtxout2){
     cout << PHWHERE << "VTX Node missing doing nothing" << endl;    
     return -1;
  }
  VtxOut *vtxout3 = findNode::getClass<VtxOut>(node3, "VtxOut");
  if (!vtxout3){
    vtxout3 = vtxout2->clone();
    PHObjectNode_t *VtxOutNode = new PHObjectNode_t(vtxout3, "VtxOut", "PHObject");
    dstNode->addNode(VtxOutNode);
  }
  // vtxout3->DeepCopy(vtxout2);
  vtxout3->Reset();
  PHIODataNode<BbcOut> *bbcoutnode;

  PHTypedNodeIterator<BbcOut> bbcout1(node1);
  bbcoutnode = bbcout1.find("BbcOut");
  BbcOut* bbc1       = bbcoutnode->getData();

  PHTypedNodeIterator<BbcOut> bbcout2(node2);
  bbcoutnode = bbcout2.find("BbcOut");

  float vtx1   = bbc1 ->get_VertexPoint();
  float t01    = bbc1 ->get_VertexPoint();
  float t0err1 = bbc1 ->get_VertexPoint();
  float vtxerr1= bbc1 ->get_VertexPoint();

  PHTypedNodeIterator<BbcOut> bbcout3(node3);
  bbcoutnode = bbcout3.find("BbcOut");
  BbcOut* bbc3       = bbcoutnode->getData();
  bbc3->set_TimeVertex(t01,t0err1,vtx1,vtxerr1);

  PHIODataNode<PHTable>* Node = (PHIODataNode<PHTable>*)iter3.findFirst("PHIODataNode","dEmcEvent");
  dEmcEventWrapper *dEmcEvent;
  if(Node){
    headerWrapper *header=0;
    PHIODataNode<PHTable>* tablenode = static_cast<PHIODataNode<PHTable>*>(iter1.findFirst("PHIODataNode","header")); 
    if(tablenode) header                 = static_cast<headerWrapper*>(tablenode->getData());
    if(header){   
      dEmcEvent=(dEmcEventWrapper *)Node->getData();
      dEmcEvent->SetRowCount(1);
      dEmcEvent->set_id(0,1);
      dEmcEvent->set_evtyp(0,1);   /* GEANT */
      dEmcEvent->set_evno(0,header->get_event(0));
      dEmcEvent->set_runno(0,header->get_run(0));
      dEmcEvent->set_serialno(0,1);
      dEmcEvent->set_impact(0,header->get_b(0));
      dEmcEvent->set_xyz(0,0,header->get_vertex(0,0));
      dEmcEvent->set_xyz(1,0,header->get_vertex(1,0));
      dEmcEvent->set_xyz(2,0,header->get_vertex(2,0));
      dEmcEvent->set_twrmultlo(0,0);
      dEmcEvent->set_twrmulthi(0,0);
      dEmcEvent->set_trigsum(0,0,0);
      dEmcEvent->set_trigsum(1,0,0);
      dEmcEvent->set_trigsum(2,0,0);
      dEmcEvent->set_tote(0,0);
      dEmcEvent->set_totet(0,0);
    }
  }
  return EVENT_OK;
}
