#include "SvxDaqQA.h"
#include <iostream>
#include "gsl/gsl_rng.h"

#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include "svxAddress.hh"
#include "RunHeader.h"
#include "EventHeader.h"
#include "PHGlobal.h"

#include <VtxOut.h>
#include <PHPoint.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TProfile.h>

using namespace std;
using namespace findNode;

//==============================================================
SvxDaqQA::SvxDaqQA(std::string &filename, int BGFlag, int agsegnum) :
   SubsysReco("SVXDAQQA") 
{
  d_OutputFileName= filename;

  init();

  cout<<"This is SvxDaqQA v4"<<endl;
}
//==============================================================

void SvxDaqQA::init() {

  
  return;
}
//==============================================================

int SvxDaqQA::Init(PHCompositeNode *topNode) {

  d_EventNumber=0;
  d_EventSeqNumber=0;


  d_OutputFile = new TFile(d_OutputFileName.c_str(),"RECREATE");

  //------------------------------------------------------------------------------------------------------ 
  // histograms
  //------------------------------------------------------------------------------------------------------ 
  d_hzbbc  =  new TH1D("hzbbc","hzbbc",800,-40,40);   //zbbc vertex;
  d_hzvtxs =  new TH1D("hzvtxs","hzvtxs",800,-40,40); //seedVertex;
  d_hzvtxp =  new TH1D("hzvtxp","hzvtxp",800,-40,40); //primaryVtx;
  d_h2zvtxpcent  = new TH2D("h2zvtxpcent","h2zvtxpcent",800,-40,40, 10,0,100); //trackVertex;
  d_h3xyvtxpcent = new TH3D("h3xyvtxpcent","h3xyvtxpcent",800,-1,1, 800,-1,1, 10,0,100); //trackVertex;
  d_h2zvtxpzbbc  = new TH2D("h2zvtxpzbbc","h2zvtxpzbbc",800,-40,40, 800, -40,40); // zbbc vs zvtxp;
  d_hcentrality =  new TH1D("hcentrality","centrality",100,0,100); //centrality;;
  d_hbbcq       =  new TH1D("hbbcq",      "bbcq",      100,0,2000); //bbc charge;;
  d_hzbbcnozvtxpcut      =  new TH1D("hzbbcnozvtxpcut","hzbbc",800,-40,40);   //zbbc vertex;
  d_hcentralitynozvtxcut =  new TH1D("hcentralitynozvtxcut","centrality before vertex cut",100,0,100); //centrality;


  for(int i=0;i<60;i++){
    d_hpixelbadpacket[i] = new TH2I(Form("hbadpixel%d",i),Form("hbadpixel%d",i),300001,-1,300000,3,-1,2); 
  }

  for(int i=0;i<40;i++){  
    d_hstripbadpacket[i] = new TH2I(Form("hbadstrip%d",i),Form("hbadstrip%d",i),300001,-1,300000,3,-1,2); 
  }

  d_h2pmodbadpkt = new TH2F("h2pmodbadpkt", "h2pixel mod vs nbadpacket per evtseq", 30000,0,30000000, 60,0,60); 
  d_h2smodbadpkt = new TH2F("h2smodbadpkt", "h2strip mod vs nbadpacket per evtseq", 30000,0,30000000, 40,0,40); 

  //-------------------------------------------------------------------
  //cout << "SvxDaqQA::Init ended." << endl;
  return EVENT_OK;
}

//==============================================================
int SvxDaqQA::InitRun(PHCompositeNode *topNode) {
  // check magnet current 
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");

  //------------SVX Address--------------------------
  d_svxadr = findNode::getClass<svxAddress>(topNode,"svxAddress");
  if (d_svxadr == NULL) {cout <<"No SvxAddress"<<endl; return DISCARDEVENT;}
  if (runheader==NULL) {
    cout << PHWHERE<< "Can't find runheader. " << endl;
    return ABORTRUN;
  }

  // run number
  d_runnumber=runheader->get_RunNumber();

  return EVENT_OK;
}
//==============================================================
int SvxDaqQA::process_event(PHCompositeNode *topNode) {
  GetNodes(topNode);


  //
  // Global Observables
  //
  d_runnumber=d_runhead->get_RunNumber();

  if((d_EventNumber%1000)==0) cout<<"event processed# "<<  d_EventNumber << "  "<<"run ="<< " "<<d_runnumber <<endl;
  d_EventSeqNumber = d_eventhead->get_EvtSequence();


  float centrality=d_global->getCentrality();

  float bbcqN = d_global->getBbcChargeN();                                                                                  
  float bbcqS = d_global->getBbcChargeS();                                                                                  
  float bbcq  = bbcqN+bbcqS;     
  
  float zvtx   =  (d_vtxout->get_Vertex()).getZ();
  float zbbc   =  (d_vtxout->get_Vertex("BBC")).getZ();

  // BBC Nhit cut
  int bbcm_n  = d_global->getBbcMultN();
  int bbcm_s  = d_global->getBbcMultS();

  if((bbcm_n<1||bbcm_s<1)) return 1;

  d_hzbbcnozvtxpcut->Fill(zbbc);
  d_hcentralitynozvtxcut->Fill(centrality);
  //filling parity error info
  for(int imodule=0;imodule<60;imodule++){
    int packetid = d_svxadr->getPixelPacketID(imodule);
    int isbadpacket = d_eventhead->isBadPacket(packetid);   
    if((d_EventNumber%100)==0)d_hpixelbadpacket[imodule]->Fill(d_EventSeqNumber/100.,isbadpacket);

    if(isbadpacket){
      d_h2pmodbadpkt->Fill(d_EventSeqNumber, imodule);
      cout<<"pixel bad : modid="<<imodule<<" "<<d_EventSeqNumber<<endl;
    }
  }

  for(int imodule=0;imodule<40;imodule++){
    int packetid = d_svxadr->getStripPacketID(imodule);
    int isbadpacket = d_eventhead->isBadPacket(packetid);   
    if((d_EventNumber%100)==0)d_hstripbadpacket[imodule]->Fill(d_EventSeqNumber/100.,isbadpacket);

    if(isbadpacket){
      d_h2smodbadpkt->Fill(d_EventSeqNumber, imodule);
    }
  }


  //
  // Vertex Cut
  //
  d_EventNumber++;
  if( (fabs(zvtx)>10)  )  return 1;    // z_vertex within +-10cm

  d_hzbbc->Fill(zbbc); 
  d_hzvtxs->Fill(zvtx); 
  d_hcentrality->Fill(centrality);
  d_hbbcq->Fill(bbcq);

  
  return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int SvxDaqQA::GetNodes(PHCompositeNode *topNode)
{
  //-------------------RunHeader----------------------------------
  d_runhead = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(d_runhead == NULL) { cerr << PHWHERE << " RunHeader node not found." << endl; return DISCARDEVENT; }
  //-------------------------------------------------------

  //-------------------EventHeader----------------------------------
  d_eventhead = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if(d_eventhead == NULL) { cerr << PHWHERE << " EventHeader node not found." << endl; return DISCARDEVENT; }
  //------------------------------------------------------------------

  //-------------------PHGlobal----------------------------------
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (!d_global) { cerr << PHWHERE << " PHGlobal node not found." << endl; return DISCARDEVENT; }
  //-------------------------------------------------------

  //------------------Vertex------------------------------
  d_vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(d_vtxout == NULL) { cerr << "VtxOut node not found." << endl; return DISCARDEVENT;}
  //-----------------------------------------------------


  
  return 0;
}

//==============================================================

int SvxDaqQA::End(PHCompositeNode *topNode) {
  
  cout << "Writing out..." << endl;
  d_OutputFile->Write();
  cout << "Closing output file..." << endl;
  d_OutputFile->Close();
  delete d_OutputFile;
  
  return 0;
}

