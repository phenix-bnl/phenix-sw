#include "TecOutV1.hh"
#include "mTecUtilities.h"
#include "PHEmbedStat.h"
#include "TecMixer.hh"
#include <iostream>
#include "Fun4AllReturnCodes.h"

using namespace std;

TecMixer::TecMixer(){
  verbose = 0;
}
TecMixer::~TecMixer(){
}
int TecMixer::InitRun( PHCompositeNode* sngl, 
		       PHCompositeNode* real, 
		       PHCompositeNode* merged) {

  if((!sngl)||(!real)||(!merged)) {
    cout << "one of the TopNode trees does not exist" << endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}

// merge Tec hits (Sasha Lebedev 24-09-2002 lebedev@iastate.edu)
int TecMixer::merge(){
  using namespace TecUtilities;
  if((!node1)||(!node2)||(!node3)) {
    cout<< "one of the TopNode trees does not exist"<<endl;
    return ABORTEVENT;
  }
  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  PHCompositeNode* embedNode = 
    static_cast<PHCompositeNode*>(iter1.findFirst("PHCompositeNode","EMBED"));
  if(!embedNode) {
    embedNode = new PHCompositeNode("EMBED");
    node1->addNode(embedNode);
  }
  PHDataNode<PHEmbedStat> *embedStatNode = 
    (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if(!embedStatNode){
    PHEmbedStat *stat = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(stat,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  //add few tables that is needed for reconstruction
  PHIODataNode<PHObject>* NodeObj = 0;
  TecOutV1* tecout = 0;

  NodeObj = (PHIODataNode<PHObject>*)iter3.findFirst("PHIODataNode","TecOutV1");
  if(NodeObj){
    tecout = (TecOutV1*)NodeObj->getData();
    tecout->Reset();
  } else {
    tecout = new TecOutV1();
    PHIODataNode<PHObject>* TecOutNodeNew = new
                       PHIODataNode<PHObject>(tecout,"TecOutV1","PHObject");
    node3->addNode(TecOutNodeNew);
  } 
  TecOut*   TEC1 = 0;
  TecOut*   TEC2 = 0;
  TecOutV1* TEC3 = 0;
  TecOut* TECHIT1 = 0;
  TecOut* TECHIT2 = 0;

  PHIODataNode<TecOut>   *tecnode = 0;
  PHIODataNode<TecOutV1> *tecnodev1 = 0;

  PHTypedNodeIterator<TecOut> tecout1(node1);
  tecnode   = tecout1.find("TecOut");
  if(!tecnode) {
    cout << PHWHERE << "MC Data does not have TecOut Node, Skip TEC" << endl;
    return  EVENT_OK;    
  }
  TEC1      = tecnode->getData();
  tecnode   = tecout1.find("TecHitOut");
  if(!tecnode) {
    cout << PHWHERE << "MC Data does not have TecHitOut Node, Skip TEC" << endl;
    return  EVENT_OK;    
  }
  TECHIT1   = tecnode->getData();
  PHTypedNodeIterator<TecOut> tecout2(node2);
  tecnode   = tecout2.find("TecOut");
  if(!tecnode) {
    cout << PHWHERE << "Real Data does not have TecOut Node, Skip TEC" << endl;
    return  EVENT_OK;    
  }
  TEC2      = tecnode->getData();
  tecnode   = tecout2.find("TecHitOut");
  if(!tecnode) {
    cout << PHWHERE << "Real Data does not have TecHitOut Node, Skip TEC" << endl;
    return  EVENT_OK;    
  }
  TECHIT2   = tecnode->getData();

  PHTypedNodeIterator<TecOutV1> tecout3(node3);
  tecnodev1 = tecout3.find("TecOutV1");
  TEC3      = tecnodev1->getData();

  int numOfHits1 = TECHIT1->getNHits();
  int numOfHits2 = TECHIT2->getNHits();

  if (verbose > 10) {
    cout << "TecMixer::merge()" << endl;
    cout << "###### mergeTEC sim : " << TECHIT1->getNHits() 
	 << " " << TEC1->getNTracks() << endl;
    cout << "###### mergeTEC real: " << TECHIT2->getNHits() 
	 << " " << TEC2->getNTracks() << endl;
  }

  TEC3->Reset();

  float xyz[3]; xyz[0]=0.;xyz[1]=0.;xyz[2]=0.;
  float charge=0.;
  int i3 = -1;
  for(int i1=0; i1<numOfHits1; i1++) {
  int found=0;
    for(int i2=0; i2<numOfHits2; i2++) {
      if(TECHIT1->getHitGlobalIndex(i1)==TECHIT2->getHitGlobalIndex(i2)) {
        found = 1;
        i3 = i2;
        break;
      }
    }
      if(!found) {
        int index = TECHIT1->getHitIndex(i1);
        int wire = TECHIT1->getHitWire(i1);
        int bin = TECHIT1->getHitTimeBin(i1);
        int adc = TECHIT1->getHitADC(i1);
        int itrack = TECHIT1->getHitTrackID(i1);
        TEC3->AddTecHit(index,wire,bin,adc,charge,xyz,itrack);
      }
      else {  // mix hits
        int index = TECHIT1->getHitIndex(i1);
        int wire = TECHIT1->getHitWire(i1);
        int bin = TECHIT1->getHitTimeBin(i1);
        int adc1 = TECHIT1->getHitADC(i1);
        int adc2 = TECHIT2->getHitADC(i3);
        int itrack = TECHIT1->getHitTrackID(i1);
	float charge1 = Ampl2Charge(adc1);
	float charge2 = Ampl2Charge(adc2);
	float charge3 = charge1+charge2;
        int adc = Charge2Ampl(charge3);  
        TEC3->AddTecHit(index,wire,bin,adc,charge,xyz,itrack);
      }
  }

  for(int i2=0; i2<numOfHits2; i2++) {
  int found=0;
    for(int i1=0; i1<numOfHits1; i1++) {
      if(TECHIT1->getHitGlobalIndex(i1)==TECHIT2->getHitGlobalIndex(i2)) {
        found = 1;
        break;
      }
    }
      if(!found) {
        int index = TECHIT2->getHitIndex(i2);
        int wire = TECHIT2->getHitWire(i2);
        int bin = TECHIT2->getHitTimeBin(i2);
        int adc = TECHIT2->getHitADC(i2);
        int itrack = TECHIT2->getHitTrackID(i2);
        TEC3->AddTecHit(index,wire,bin,adc,charge,xyz,itrack);
      }
      else {  // hits already mixed
              // do nothing
      }
  }

  if (verbose > 10) {
    cout << PHWHERE << endl;
    cout << "###### mergeTEC: FINAL number of hits and tracks: " << endl;
    cout << "TEC1 (sim) : " << TECHIT1->getNHits() << " " << TEC1->getNTracks() << endl;
    cout << "TEC2 (real): " << TECHIT2->getNHits() << " " << TEC2->getNTracks() << endl;
    cout << "TEC3 (mix) : " << TEC3->getNHits() << " " << TEC3->getNTracks() << endl;
  }
  
  return EVENT_OK;
}
