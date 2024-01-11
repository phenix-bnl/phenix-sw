/*
Use this to cut events before Shower module, or your analysis
Author: seto
Cuts: 
1) Stack=1 (no options)
2) Vertex
3) Trigger
4) number of hits
Later other options might be added. The stack=1 cut is the only one implemented by default. Spin containers and MPC are here to ease the adddition of other cuts
*/
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
using std::vector;
using std::string;

// PHENIX stuff
#include "getClass.h"
#include "PHCompositeNode.h"
#include "TriggerHelper.h"
#include "Fun4AllReturnCodes.h"
#include "BbcOut.h"
#include "PHGlobal.h"

// MpcEx stuff
#include "MpcExEventHeader.h"
#include "MpcExRawHit.h"
#include "mMpcExQualityCut.h"

using namespace findNode;

//====================================================
mMpcExQualityCut::mMpcExQualityCut( const char* name ) : 
  SubsysReco( name )
{
  fHits = NULL;
  fMpcExEventHeader = NULL;
  printinterval=-1;
  maxhits=-1;
  zvertexlimit=-1.;
  nevents_tot=0;
  nevents_pass_stackcut=0;
  nevents_pass_vertexcut=0;
  nevents_pass_triggercut=0;
  nevents_pass_hitscut=0;
  nevents_pass=0;

  using_set_list=false;
  using_All=false;

  // init set_trigger_list. I don't know if I have to do this to pass the GateKeeper
  set_trigger_list.push_back("");

  // Add triggers here. I have added Run15 pp200, pAu200
  // MinBias triggers
  trigger_list.push_back("BBCLL1(>0 tubes)");
  trigger_list.push_back("BBCLL1(>0 tubes) narrowvtx");
  trigger_list.push_back("BBCLL1(>0 tubes) novertex");
  trigger_list.push_back("BBCLL1(noVtx)&(ZDCN||ZDCS)");
  trigger_list.push_back("ZDCLL1wide");
  trigger_list.push_back("ZDCNS");
  trigger_list.push_back("ZDCN||ZDCS");
  trigger_list.push_back("BBCLL1(>0 tubes)");
  trigger_list.push_back("BBCLL1(>0 tubes)_central35_narrowvtx");
  trigger_list.push_back("BBCLL1(>0 tubes)_central25_narrowvtx");
  trigger_list.push_back("BBCLL1(>0 tubes) narrowvtx");
  trigger_list.push_back("BBCLL1(>0 tubes) novertex");
  trigger_list.push_back("ZDCLL1wide");
  trigger_list.push_back("ZDCN");
  trigger_list.push_back("ZDCN_BL");
  trigger_list.push_back("ZDCN_BL");
  trigger_list.push_back("ZDCNS");
  trigger_list.push_back("ZDCNS");
  trigger_list.push_back("ZDCS");
  trigger_list.push_back("ZDC_N&&FVTX_HighMult_N");
  // MPC triggers
  trigger_list.push_back("MPC_N_B");
  trigger_list.push_back("MPC_S_B");
  trigger_list.push_back("MPC_N_C&ERT_2x2");
  trigger_list.push_back("MPC_S_C&ERT_2x2");
  trigger_list.push_back("MPC_N_C&ERTLL1_2x2");
  trigger_list.push_back("MPC_S_C&ERTLL1_2x2");
  trigger_list.push_back("MPC_N_C&MPC_N_C");
  trigger_list.push_back("MPC_S_C&MPC_S_C");
  trigger_list.push_back("MPC_N_C&MPC_S_C");
  trigger_list.push_back("MPC_N_S_A");
  trigger_list.push_back("MPC_N_A");
  trigger_list.push_back("MPC_S_A");
  // high multiplicity FVTX trigger
  trigger_list.push_back("FVTX_HighMult_N");
  trigger_list.push_back("FVTX_HighMult_S");
  trigger_list.push_back("FVTX_HighMult_N_AND_S&BBCLL1narrow");
  trigger_list.push_back("FVTX_HighMult_N_OR_S&BBCLL1narrow");
  trigger_list.push_back("FVTX_HighMult_N&BBCLL1narrow");
  trigger_list.push_back("FVTX_HighMult_N&BBCLL1(narrow)");
  trigger_list.push_back("FVTX_HighMult_S&BBCLL1narrow");
  trigger_list.push_back("FVTX_HighMult_S&BBCLL1(narrow)");

  for(unsigned int i=0; i<trigger_list.size(); i++){
    set_trigger_list_count.push_back(0);
  }  
}
//====================================================
int mMpcExQualityCut::End(PHCompositeNode *topNode)
{
  std::cout<<"  "<<std::endl;
  std::cout<<" --------------- Statistics from MpcExQualityCut ------------------------- "<<std::endl;
  std::cout<<" Total events coming in to quality cut = "<<nevents_tot<<std::endl;
  std::cout<<" Number passing stack cut = "<<nevents_pass_stackcut<<std::endl;
  std::cout<<" Number passing vertex cut = "<<nevents_pass_vertexcut<<"    Z vertex cut = "<<zvertexlimit<<std::endl;
  std::cout<<" Number passing triggger cut = "<<nevents_pass_triggercut<<std::endl;
  std::cout<<" Number passing max hits cut = "<<nevents_pass_hitscut<<"  max hits cut set to = "<<maxhits<<std::endl;
  std::cout<<" Total number passing quality cut = "<<nevents_pass<<std::endl;
  std::cout<<"  "<<std::endl;

  std::cout<<" --------------- counts from MpcExQualityCut: triggers with count>0 -------------------- "<<std::endl;
  if(using_set_list && !using_All){
    for(unsigned i=0; i<set_trigger_list.size(); i++){
      if(set_trigger_list_count[i]>0)std::cout<<set_trigger_list[i]<<" =  "<<set_trigger_list_count[i]<<std::endl;
    }
  }
  std::cout<<"  "<<std::endl;
  return EVENT_OK;
}
//====================================================
mMpcExQualityCut::~mMpcExQualityCut()
{
}
//====================================================
int mMpcExQualityCut::Init(PHCompositeNode* top_node)
{
  return EVENT_OK; 
}
//====================================================
int mMpcExQualityCut::InitRun(PHCompositeNode* top_node)
{
  return EVENT_OK; 
}
//====================================================
int mMpcExQualityCut::process_event(PHCompositeNode* top_node) {
  nevents_tot++;
  if(printinterval>0 && nevents_tot%printinterval==0){
    std::cout<<" nevents = "<<nevents_tot<<std::endl;
  }

  //  First the stack and other data quality cuts
  fMpcExEventHeader = getClass<MpcExEventHeader>(top_node,"MpcExEventHeader");
  if(!fMpcExEventHeader) { 
    std::cout<<PHWHERE<<" No EventHeader, exiting."<<std::endl; 
    return ABORTRUN;
  }
  bool EvtOK = IsEventOK();
  if (!EvtOK) return ABORTEVENT;

  nevents_pass_stackcut++;

  // next the vertex cuts
  PHGlobal *phglobal = getClass<PHGlobal> (top_node, "PHGlobal");
  BbcOut *bbcout     = getClass<BbcOut>   (top_node, "BbcOut");
  if(!bbcout || !phglobal){ std::cout << "No BbcOut or PHGlobal!  No sense continuing" << std::endl; return ABORTRUN;}

  float zvertex = phglobal->getBbcZVertex();
  
  if(zvertexlimit<999. && zvertexlimit>0. && std::abs(zvertex)>zvertexlimit) return ABORTEVENT; 

  nevents_pass_vertexcut++;
  
  bool gottrig = EvaluateTrigger(top_node);
  if( !gottrig ) return ABORTEVENT; 

  nevents_pass_triggercut++;

  if(maxhits>0){
    fHits = getClass<MpcExRawHit>(top_node, "MpcExRawHit");
    if(!fHits) { std::cout<<PHWHERE<<" No MpcExRawHit, exiting."<<std::endl; return ABORTRUN; }
    int nhits = fHits->getnhits();
    if(nhits>maxhits)return ABORTEVENT;
  }

  nevents_pass_hitscut++;

  nevents_pass++;

  return EVENT_OK;
}
//====================================================
bool  mMpcExQualityCut::IsEventOK() 
{
  if (fMpcExEventHeader->getStack() > 1) return false;

  return true;
}
//====================================================
bool mMpcExQualityCut::EvaluateTrigger(PHCompositeNode *top_node) {

  if(!using_set_list || using_All) return true;

  TriggerHelper *myTH = new TriggerHelper(top_node);
  bool gottrigger=false;
  for(unsigned i=0; i<set_trigger_list.size(); i++){
    if(myTH->trigScaled(set_trigger_list[i])){
      gottrigger=true;
      set_trigger_list_count[i]++;
    }
    //    if(myTH->trigScaled(set_trigger_list[i]))std::cout<<" got a trigger i="<<i<<" "<<myTH->trigScaled(set_trigger_list[i])<<" "<<set_trigger_list[i]<<std::endl;
  }
  delete myTH;   
  
  return gottrigger;
}
//====================================================
void mMpcExQualityCut::SetZVertexLimit(float vl) {
  zvertexlimit=vl; 
  std::cout<<" Setting mMpcExQualityCut::zvertexlimit = "<<zvertexlimit<<std::endl;
  return;
}
//====================================================
void mMpcExQualityCut::SetTrigger(string trig) {
  using_set_list=true;
  // first see if you want all
  if(trig=="All"){
    using_All=true;
    std::cout<<" Accept All triggers; this command trumps all other set trigger calls. It will not keep a trigger count "<<std::endl;
    return;
  }

  // make sure trigger exists
  bool trigger_exists = false;
  for (unsigned int i=0; i<trigger_list.size(); i++){
    if(trigger_list[i]==trig)trigger_exists=true;
  }
  if(!trigger_exists){
    std::cout<<PHWHERE<<" you have a problem - the trigger you asked for does not exis, exiting... You asked for: "<<trig<<std::endl;
    std::cout<<" Allowed trigger list: " <<std::endl;
    std::cout<<"All"<<std::endl;
    for(unsigned i=0; i<trigger_list.size(); i++){
      std::cout<<trigger_list[i]<<std::endl;
    }
    return;
  }

  // add the trigger to your list  
  if(set_trigger_list[0]==""){
    set_trigger_list[0]=trig;
  }else{
    set_trigger_list.push_back(trig);
  }
  std::cout<<" Setting mMpcExQualityCut::SetTrigger = "<<trig<<std::endl;
  return;
}
//====================================================
void mMpcExQualityCut::SetMaxHits(int mh ) {
  maxhits=mh;
  std::cout<<" Setting mMpcExQualityCut::SetMaxHits = "<<maxhits<<std::endl;
  return;
}
//====================================================
void mMpcExQualityCut::SetPrintInterval(int prt ) {
  printinterval=prt;
  std::cout<<" Setting mMpcExQualityCut::printinterval = "<<printinterval<<std::endl;
  return;
}
//====================================================
