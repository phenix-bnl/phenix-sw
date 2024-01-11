#include "AccRaw.h"
#include "PHIODataNode.h"

#include "PHEmbedStat.h"
#include "AccMixer.hh"
#include "Fun4AllReturnCodes.h"

#include <iostream>

using namespace std;


int AccMixer::InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}
// merge Acc hits (Narumi Kurihara 12-05-2005 narumi@cns.s.u-tokyo.ac.jp)
int AccMixer::merge(){
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
    PHEmbedStat *event = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(event,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  AccRaw* ACCREC1;
  AccRaw* ACCREC2;
  AccRaw* ACCREC3;
  PHIODataNode<AccRaw> *accnode;
  PHTypedNodeIterator<AccRaw> accout1(node1);
  PHTypedNodeIterator<AccRaw> accout2(node2);
  PHTypedNodeIterator<AccRaw> accout3(node3);

  accnode        = accout1.find("AccRaw");
  ACCREC1        = accnode->getData(); 
  accnode        = accout2.find("AccRaw");
  ACCREC2        = accnode->getData(); 
  accnode        = accout3.find("AccRaw");
  ACCREC3        = accnode->getData(); 

  vector<int>& accrawE    = embedStat->get_accrawEmbed();
  vector<int>& accrawStat = embedStat->get_accrawEmbedStat();
  accrawE.clear();          accrawStat.clear();
  if(ACCREC1){
    int nentries = ACCREC1->get_nraw();
    accrawE.resize(nentries,-1);                 accrawStat.resize(nentries,-1);
  }

  int numOfAcc1                     = ACCREC1->get_nraw();
  int numOfAcc2                     = ACCREC2->get_nraw();
  //COPY from ACCREC2 to ACCREC3
  ACCREC3->set_TClonesArraySize(numOfAcc2);
  for(int j=0;j<numOfAcc2;j++){
    ACCREC3->set_adc(j,0,ACCREC2->get_adc(j,0));
    ACCREC3->set_adc(j,1,ACCREC2->get_adc(j,1));
    ACCREC3->set_adcpost(j,0,ACCREC2->get_adcpost(j,0));
    ACCREC3->set_adcpost(j,1,ACCREC2->get_adcpost(j,1));
    ACCREC3->set_adcpre(j,0, ACCREC2->get_adcpre(j,0));
    ACCREC3->set_adcpre(j,1, ACCREC2->get_adcpre(j,1));
    ACCREC3->set_tdc(j,0,ACCREC2->get_tdc(j,0));
    ACCREC3->set_tdc(j,1,ACCREC2->get_tdc(j,1));
  }
  //finish copying


  int numOfAcc3                     = ACCREC3->get_nraw();
  //  int numOfAcc1 = 160;//ACC::ACC_NPMT_HALF;
  //  int numOfAcc3 = 160;//ACC::ACC_NPMT_HALF;

  // node3 is already copied from node2 in PHEmbedMixer::event()
  // now we merge two tables

  ACCREC3->set_TClonesArraySize(numOfAcc3);// AccRaw Table Size is same between Simulation and Real
  int mergedd, mergedid;
  //Do the embedding work!!!!
  for(int i=0;i<numOfAcc1;i++){
    mergedid =-1;
    mergedd  = 0;
    for(int j =0;j<numOfAcc3;j++){
      if(ACCREC1->get_boxid(i) == ACCREC3->get_boxid(j)){ //overlapped

	//node1 info
	float adc1_1 = ACCREC1->get_adc(i,0);
	float adc2_1 = ACCREC1->get_adc(i,1);
	//float adcpost1_1 = ACCREC1->get_adcpost(i,0);
	//float adcpost2_1 = ACCREC1->get_adcpost(i,1);
	float tdc1_1  = ACCREC1->get_tdc(i,0);
	float tdc2_1  = ACCREC1->get_tdc(i,1);

	// node3 info.
	float adc1_3 = ACCREC3->get_adc(j,0);
	float adc2_3 = ACCREC3->get_adc(j,1);
	float adcpost1_3 = ACCREC3->get_adcpost(j,0);
	float adcpost2_3 = ACCREC3->get_adcpost(j,1);
	float adcpre1_3 = ACCREC3->get_adcpre(j,0);
	float adcpre2_3 = ACCREC3->get_adcpre(j,1);
	float tdc1_3  = ACCREC3->get_tdc(j,0);
	float tdc2_3  = ACCREC3->get_tdc(j,1);

	if( ( adc1_1 > 0 && adc1_1 < 1024 ) || (adc2_1 >0 && adc2_1 < 1024) 
	   || (tdc1_1 > 0 && tdc1_1 < 1024 ) || (tdc2_1 >0 && tdc2_1 < 1024 ) ) {
	  if(verbose>6){
	    cout<<"ACC hits are overlapped in one slat"<<endl;
	    cout<<"ACC boxid = "<<ACCREC3->get_boxid(j)<<endl;
	  }

	  mergedd |= 0x01;
	  //+----------------------------------+
	  //| Calculate overlap effect for ACC |
	  //+----------------------------------+

	  // select/merge for each PMT
	  float T1, T2, Q1, Q2, Qpost1, Qpost2;
	  Qpost1 = (adcpost1_3 + adc1_1);
	  Q1 = Qpost1 - adcpre1_3; // charge sum.
	  if( Qpost1 > 1023 ) { //Max ADC channel = 1023
	    Q1 = 1023 - adcpre1_3;
	    Qpost1 = 1023;
	  }
	  Qpost2 = (adcpost2_3 + adc2_1);
	  Q2 = Qpost2 - adcpre1_3; // charge sum.
	  if( Qpost2 > 1023 ) {
	    Q2 = 1023 - adcpre2_3;
	    Qpost2 = 1023;
	  }
	  
	  if(tdc1_1< tdc1_3 ) T1 = tdc1_3; // select early timing.
	  else T1 = tdc1_1;
	  if(tdc2_1< tdc2_3 ) T2 = tdc2_3; // select early timing.
	  else T2 = tdc2_1;
	  
	  // These values put in node3
	  ACCREC3->set_adc(j,0,(int)Q1);
	  ACCREC3->set_adc(j,1,(int)Q2);
	  ACCREC3->set_adcpost(j,0,(int)Qpost1);
	  ACCREC3->set_adcpost(j,1,(int)Qpost2);
	  ACCREC3->set_tdc(j,0,(int)T1);
	  ACCREC3->set_tdc(j,1,(int)T2);
	  
	  mergedid = ACCREC3->get_boxid(j);
	  
	  if(verbose>6){
	    cout<<"--------------------------------------------------"<<endl;
	    cout<<"overlap:   singleMC + real ==> merged"<<endl;
	    cout<<"  adc1      :   "<<adc1_1<<"   + "<<adc1_3<<" ==> "<<Q1<<endl;
	    cout<<"  adc2      :   "<<adc2_1<<"   + "<<adc2_3<<" ==> "<<Q2<<endl;
	    cout<<"  adcpost1  :   "<<adc1_1<<"   + "<<adcpost1_3<<" ==> "<<Qpost1<<endl;
	    cout<<"  adcpost1  :   "<<adc2_1<<"   + "<<adcpost2_3<<" ==> "<<Qpost2<<endl;
	    cout<<"  tdc1      :   "<<tdc1_1<<"   | "<<tdc1_3<<" ==> "<<T1<<endl;
	    cout<<"  tdc2      :   "<<tdc2_1<<"   | "<<tdc2_3<<" ==> "<<T2<<endl;
	    cout<<"--------------------------------------------------"<<endl;
	  }
	}
      }
    }

    accrawE[i]    = mergedid;
    accrawStat[i] = mergedd;
  }

  return EVENT_OK;

}
