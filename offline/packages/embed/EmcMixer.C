#include "emcTowerContainer.h"
//INCLUDECHECKER: Removed this line: #include "emcClusterContainer.h"
#include "emcTowerContent.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHIODataNode.h"
#include "PHEmbedStat.h"
#include "EmcMixer.hh"
#include <iostream>
#include "Fun4AllReturnCodes.h"
#include "emcNodeHelper.h"
#include "emcClusterContainerv5.h"

using namespace std;

int EmcMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}
// merge Emc hits (Sasha Lebedev 24-09-2002 lebedev@iastate.edu)
int EmcMixer::merge(){
  if((!node1)||(!node2)||(!node3)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }
  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  PHCompositeNode*embedNode = static_cast<PHCompositeNode*>(iter1.findFirst("PHCompositeNode","EMBED"));
  if(!embedNode){
    embedNode = new PHCompositeNode("EMBED");
    node1->addNode(embedNode);
  }
  PHDataNode<PHEmbedStat> *embedStatNode = (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if(!embedStatNode){
    PHEmbedStat *stat = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(stat,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  PHCompositeNode* dstNode = dynamic_cast<PHCompositeNode*>(iter3.findFirst("PHCompositeNode", "DST"));
  if (! dstNode){
    dstNode = new PHCompositeNode("DST");
    node3->addNode(dstNode);
  }


  //reset the higher level nodes
  PHIODataNode<emcClusterContainer> *clusnode2,*clusnode3;
  emcClusterContainer*CLUS2,*CLUS3;
  PHTypedNodeIterator<emcClusterContainer> clus2(node2);
  PHTypedNodeIterator<emcClusterContainer> clus3(node3);
  clusnode2    = clus2.find("emcClusterContainer");
  clusnode3    = clus3.find("emcClusterContainer");
  if(!clusnode3&&clusnode2){
    CLUS2        = clusnode2->getData();
    CLUS3        = CLUS2->clone();
    PHIODataNode<PHObject> *clusNode =  new PHIODataNode<PHObject>(CLUS3, "emcClusterContainer", "PHObject");
    dstNode->addNode(clusNode);
  }else if(!clusnode3){
    cout<<" didn't find emcClusterContainer, so I add emcClusterContainerv5"<<endl;
    emcNodeHelper nh;
    nh.addObject<emcClusterContainerv5>(node3,"emcClusterContainer");
  }
  clusnode3    = clus3.find("emcClusterContainer");
  if(clusnode3){
    CLUS3        = clusnode3->getData();
    CLUS3->Reset();
  }

  emcTowerContainer *HIT1;
  emcTowerContainer *HIT2;
  emcTowerContainer *HIT3;

  PHIODataNode<emcTowerContainer> *hitnode1,*hitnode2,*hitnode3;
  PHTypedNodeIterator<emcTowerContainer> hit1(node1);
  hitnode1     = hit1.find("emcTowerContainer");

  HIT1        = hitnode1->getData();
  PHTypedNodeIterator<emcTowerContainer> hit2(node2);

  hitnode2     = hit2.find("emcTowerContainer");
  HIT2        = hitnode2->getData();

  PHTypedNodeIterator<emcTowerContainer> hit3(node3);
  hitnode3     = hit3.find("emcTowerContainer");

  if(!hitnode3){
    HIT3 = HIT2->clone();
    PHIODataNode<PHObject> *towerNode =  new PHIODataNode<PHObject>(HIT3, "emcTowerContainer", "PHObject");
    dstNode->addNode(towerNode);
  }else{
    HIT3        = hitnode3->getData();
  }
  
  int numOfHit1    = HIT1->size();
  int numOfHit2    = HIT2->size();
  //COPY from HIT2 to HIT3
  HIT3->Reset();
  for(int i=0;i<numOfHit2;i++){
    emcTowerContent * hit2 = HIT2->getTower(i);
    emcTowerContent*tmp = HIT3->addTower(i,*hit2);
    if(!tmp){
      cout<<"EMCal: Wrong"<<endl;
    }
  }
  //finish copying
  int numOfHit3    = HIT3->size();

  
  vector<int>& emctowerE    = embedStat->get_emctowerEmbed();
  vector<int>& emctowerStat = embedStat->get_emctowerEmbedStat();
  emctowerE.clear();                            emctowerStat.clear();
  if(HIT1){
    int nentries = HIT1->size();
    emctowerE.resize(nentries,-1);              emctowerStat.resize(nentries,-1);
  }

  //  cout << endl << endl << "IN EMC MERGER" << endl << endl;
  
  int total = numOfHit3;
  int overlap = 0;
  float energy,tof;
  for(int i=0;i<numOfHit1;i++){
    emcTowerContent * hit1 = HIT1->getTower(i);
    emcTowerContent * hit3 = HIT3->findTower(hit1->TowerID());
    overlap = 0;
    if(hit3){//merge
      overlap =1;
      energy = hit3->Energy() + hit1->Energy();
      tof = hit3->ToF();
      if(tof>hit1->ToF())  tof =  hit1->ToF();
      unsigned int dead = hit3->ErrorNeighbours() | hit1->ErrorNeighbours();
      unsigned int warn = hit3->WarnNeighbours()  | hit1->WarnNeighbours();
      cout<<"merged:"<<endl;
      cout<<"energy: "<<hit3->Energy()<<" "<<hit1->Energy()<<" "<<energy<<endl;
      cout<<"tof: "<<hit3->ToF()<<" "<<hit1->ToF()<<" "<<tof<<endl;

      hit3->SetCalibrated(energy,tof);
      hit3->SetNeighbours(dead,warn);
      emctowerE[i]    = hit1->TowerID();
      emctowerStat[i] = overlap;
    }else{//no merge
     //hit1 is emcClusterContentv1S
      //HIT3->addTower(total,*hit1);
      hit3 = HIT3->addTower(total);
      hit3->Zero();
      hit3->SetCalibrated(hit1->Energy(),hit1->ToF());
      hit3->SetID(hit1->FEM(),hit1->Channel());
      hit3->SetADCTDC(hit1->ADC(),hit1->TDC());
      hit3->SetNeighbours(hit1->ErrorNeighbours(),hit1->WarnNeighbours());

      emctowerE[i]    = hit1->TowerID();
      emctowerStat[i] = overlap;
      total++;
    }
  }
  //  cout << endl << "DONE EMC MERGER" << endl << endl;
  return EVENT_OK;
}
