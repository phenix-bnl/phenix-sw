#include "MutrgSimreco.hh"

#include "Fun4AllReturnCodes.h"

#include "MutrgEmulateHeader.hh"
#include "MutrgEmulateHit.hh"

////////////////////////////////////////////////////////////

MutrgSimreco::MutrgSimreco(const char *name) : SubsysReco(name){
  flag_emu_header=true;
  flag_emu_hit=true;
  mutrg_emu_header=NULL;
  mutrg_emu_hit=NULL;
  CreateModule();
}

//////////////////////////////////////////////////////////////

MutrgSimreco::~MutrgSimreco(void){
  if(mutrg_emu_header){delete mutrg_emu_header; mutrg_emu_header=NULL;}
  if(mutrg_emu_hit){delete mutrg_emu_hit; mutrg_emu_hit=NULL;}
}

//////////////////////////////////////////////////////////////

int MutrgSimreco::Init(PHCompositeNode *top_node){
  if(flag_emu_header){
    if(mutrg_emu_header->Init(top_node)){return ABORTRUN;}
  }

  if(flag_emu_hit){
    if(mutrg_emu_hit->Init(top_node)){return ABORTRUN;}
  }

  return 0;
}

/////////////////////////////////////////////////////////////

int MutrgSimreco::InitRun(PHCompositeNode *top_node){
  if(flag_emu_header){
    if(mutrg_emu_header->InitRun(top_node)){return ABORTRUN;}
  }

  if(flag_emu_hit){
    if(mutrg_emu_hit->InitRun(top_node)){return ABORTRUN;}
  }

  return 0;
}

/////////////////////////////////////////////////////////////

int MutrgSimreco::process_event(PHCompositeNode *top_node){
  if(flag_emu_header){
    if(mutrg_emu_header->ProcessEvent(top_node)){return ABORTRUN;}
  }

  if(flag_emu_hit){
    if(mutrg_emu_hit->ProcessEvent(top_node)){return ABORTRUN;}
  }

  return EVENT_OK;
}

/////////////////////////////////////////////////////////////

void MutrgSimreco::CreateModule(void){
  if(!mutrg_emu_header){mutrg_emu_header=new MutrgEmulateHeader();}
  if(!mutrg_emu_hit){mutrg_emu_hit=new MutrgEmulateHit();}
  return;
}

///////////////////////////////////////////////////////////////
