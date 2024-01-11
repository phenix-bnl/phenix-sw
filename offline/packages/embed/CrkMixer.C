#include "dCrkHitWrapper.h"
#include "CrkHit.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "PHEmbedStat.h"
#include "CrkMixer.hh"
#include "CrkRing.h"
#include <iostream>
#include "Fun4AllReturnCodes.h"

using namespace std;

int CrkMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}
// merge Crk hits (Sasha Lebedev 24-09-2002 lebedev@iastate.edu)
int CrkMixer::merge(){
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

  PHIODataNode<PHTable>* dCrkHitNode;
  dCrkHitNode = (PHIODataNode<PHTable>*)iter1.findFirst("PHIODataNode","dCrkHit");
  if(!dCrkHitNode){
    dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit", 5120);
    dCrkHitNode = new PHIODataNode<PHTable>(dCrkHit, "dCrkHit");
    node1->addNode(dCrkHitNode);
  }
  dCrkHitNode = (PHIODataNode<PHTable>*)iter2.findFirst("PHIODataNode","dCrkHit");
  if(!dCrkHitNode){
    dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit", 5120);
    dCrkHitNode = new PHIODataNode<PHTable>(dCrkHit, "dCrkHit");
    node2->addNode(dCrkHitNode);
  }
  dCrkHitNode = (PHIODataNode<PHTable>*)iter3.findFirst("PHIODataNode","dCrkHit");
  if(!dCrkHitNode){
    dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit", 5120);
    dCrkHitNode = new PHIODataNode<PHTable>(dCrkHit, "dCrkHit");
    node3->addNode(dCrkHitNode);
  }
  //reset the rings
  PHIODataNode<CrkRing> *crkringnode;
  CrkRing*crkring=0;
  PHTypedNodeIterator<CrkRing> crkring3(node3);
  crkringnode = crkring3.find("CrkRing");
  if(crkringnode){
    crkring        = crkringnode->getData();
    crkring->Reset();
  }else{
    cout<<"ring node not exist"<<endl;
  }

  CrkHit *HIT1;
  CrkHit *HIT2;
  CrkHit *HIT3;
  PHIODataNode<CrkHit> *crkhitnode;
  PHTypedNodeIterator<CrkHit> crkhit1(node1);
  crkhitnode = crkhit1.find("CrkHit");
  HIT1       = crkhitnode->getData();

  PHTypedNodeIterator<CrkHit> crkhit2(node2);
  crkhitnode = crkhit2.find("CrkHit");
  HIT2       = crkhitnode->getData();

  PHTypedNodeIterator<CrkHit> crkhit3(node3);
  crkhitnode = crkhit3.find("CrkHit");
  HIT3       = crkhitnode->getData();

  int numOfHit1    = HIT1->get_CrkNHit();
  int numOfHit2    = HIT2->get_CrkNHit();
  //COPY from HIT2 to HIT3
  HIT3->set_TClonesArraySize(numOfHit2); // Allocates space, but doesn't set # hits!
  HIT3->set_CrkNHit(numOfHit2); // New 7/20/2009 - sets CrkNHit. No merging happens w/o this line!

  for(int i=0;i<numOfHit2;i++){
    HIT3->AddCrkHit(i);
    HIT3->set_pmt(i,HIT2->get_pmt(i));
    HIT3->set_npe(i,HIT2->get_npe(i));
    HIT3->set_time(i,HIT2->get_time(i));
  }
  //finish copying

  int numOfHit3    = HIT3->get_CrkNHit();

  vector<int>& crkhitE       = embedStat->get_crkhitEmbed();
  vector<int>& crkhitStat    = embedStat->get_crkhitEmbedStat();
  crkhitE.clear();                              crkhitStat.clear();
  if(HIT1){
    int nentries = HIT1->get_CrkNHit();
    crkhitE.resize(nentries,-1);             
    crkhitStat.resize(nentries,-1);
  }

  HIT3->set_TClonesArraySize(numOfHit3 + numOfHit1);
  int total = numOfHit3;
  int overlap = 0;
  int mergeID = 0;

  for(int i=0;i<numOfHit1;i++){ // MC hit tubes
    overlap =0;
    for(int j =0;j<numOfHit3;j++){ // RD hit tubes (at this point) 
      if( HIT1->get_pmt(i) == HIT3->get_pmt(j)){ // if MC & RD PMT IDs match
	overlap = 1;
	mergeID = j;
	if (verbose > 30) cout << "RICH PMT overlap!" << endl;
	break;
      }
    }
    if(!overlap){
      HIT3->AddCrkHit(total);
      HIT3->set_pmt(total,HIT1->get_pmt(i));
      HIT3->set_npe(total,HIT1->get_npe(i));
      HIT3->set_time(total,HIT1->get_time(i));
      crkhitE[i]    = total;
      crkhitStat[i] = overlap;
      total++;
    }
    else { // In the merged node, keep whichever hit sooner -- AMA 2/26
      HIT3->set_npe(mergeID, HIT3->get_npe(mergeID)+HIT1->get_npe(i));
      if ( HIT3->get_time(mergeID) > HIT1->get_time(i)){
	HIT3->set_time(mergeID, HIT1->get_time(i));
      }
      crkhitE[i]    = mergeID;
      crkhitStat[i] = overlap;
    }
  }

  HIT3->set_CrkNHit(total);
  
  //now make a copy to old dCrkHit table because CrkPID needs it
  numOfHit3 = total;

  if (verbose > 30) {
    cout << "# hit RICH PMTs (RD, MC, EMB): " 
	 << numOfHit1 << " " << numOfHit2 << " " << numOfHit3 << endl;
  }

  PHIODataNode<dCrkHitWrapper> *crkhitoldnode;
  PHTypedNodeIterator<dCrkHitWrapper> crkhitold1(node1);
  crkhitoldnode = crkhitold1.find("dCrkHit");
  dCrkHitWrapper*HITOLDW1       = crkhitoldnode->getData();

  PHTypedNodeIterator<dCrkHitWrapper> crkhitold3(node3);
  crkhitoldnode = crkhitold3.find("dCrkHit");
  dCrkHitWrapper*HITOLDW3       = crkhitoldnode->getData();

  HITOLDW1->SetRowCount(numOfHit1);
  HITOLDW3->SetRowCount(numOfHit3);

  DCRKHIT_ST* HITOLD1 = HITOLDW1->TableData();
  DCRKHIT_ST* HITOLD3 = HITOLDW3->TableData();
  for(int i=0;i<numOfHit1;i++){
    HITOLD1[i].pmt  = HIT1->get_pmt(i);
    HITOLD1[i].npe  = HIT1->get_npe(i);
    HITOLD1[i].time = HIT1->get_time(i);
  }
  for(int i=0;i<numOfHit3;i++){
    HITOLD3[i].pmt  = HIT3->get_pmt(i);
    HITOLD3[i].npe  = HIT3->get_npe(i);
    HITOLD3[i].time = HIT3->get_time(i);
  }
  //  cout << endl << endl << "IN CRK MERGER" << endl << endl;
  return EVENT_OK;
}
