#include "MutrgProcessHit.hh"

#include "MutrgDataProcessUtil.hh"
#include "MutrgHeaderArray.hh"
#include "MutrgHitArray.hh"

using namespace std;

/////////////////////////////////////////////////////////////////

MutrgProcessHit::MutrgProcessHit(bool init_flag){
  class_name="MutrgProcessHit";
  mutrg_hits=NULL;
  mutrg_headers=NULL;
  if(init_flag){CreateObject();} // MutrgProcessHit::CreateObject
}

////////////////////////////////////////////////////////////////

MutrgProcessHit::~MutrgProcessHit(void){
  // commented out by suggestion from Chris (2009Apr12)
  //if(mutrg_hits){delete mutrg_hits;}
  //if(mutrg_headers){delete mutrg_headers;}
  //mutrg_hits=NULL;
  //mutrg_headers=NULL;
}

//////////////////////////////////////////////////////////////////

void MutrgProcessHit::CreateObject(void){
  if(!mutrg_hits){mutrg_hits=new MutrgHitArray("MutrgHit");}
  if(!mutrg_headers){mutrg_headers=new MutrgHeaderArray("MutrgHeader");}
  return;
}

//////////////////////////////////////////////////////////////////

int MutrgProcessHit::Init(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"Init");
  return 0;
}

///////////////////////////////////////////////////////////////////

int MutrgProcessHit::InitRun(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"InitRun");
  return 0;
}

///////////////////////////////////////////////////////////////////

int MutrgProcessHit::ProcessEvent(PHCompositeNode *node){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"ProcessEvent");
  return 0;
}

////////////////////////////////////////////////////////////////

int MutrgProcessHit::SetMutrgHitArray(MutrgHitArray *hits,bool flag_delete){
  return MutrgDataProcessUtil::SetMutrgObject(mutrg_hits,hits,flag_delete);
}

///////////////////////////////////////////////////////////////////

int MutrgProcessHit::SetMutrgHeaderArray(MutrgHeaderArray *headers,
					 bool flag_delete){
  return MutrgDataProcessUtil::SetMutrgObject(mutrg_headers,
					      headers,flag_delete);
}

///////////////////////////////////////////////////////////////////

MutrgHitArray*
MutrgProcessHit::RegMutrgHitArray(PHCompositeNode *node,
				  const char *name,const char *rename){
  return MutrgDataProcessUtil::RegMutrgObject(mutrg_hits,node,name,rename);
}

////////////////////////////////////////////////////////////////

MutrgHeaderArray*
MutrgProcessHit::RegMutrgHeaderArray(PHCompositeNode *node,
				     const char *name,const char *rename){
  return MutrgDataProcessUtil::RegMutrgObject(mutrg_headers,node,name,rename);
}

////////////////////////////////////////////////////////////////

int MutrgProcessHit::FillHit(PHCompositeNode *node){
  typedef PHDataNode<Event> EventNode_t;

  PHNodeIterator iter(node);
  EventNode_t* evt_node=
    dynamic_cast<EventNode_t*>(iter.findFirst("PHDataNode","PRDF"));
  if(!evt_node){
    printf("Error - %s::FillHit : No Event Node\n",ClassName());
    return -1;
  }
  
  Event *evt=evt_node->getData();
  if(!evt){
    printf("Error - %s::FillHit : Couldn't find Event\n",ClassName());
    return -1;
  }

  return FillHit(evt);
}

//////////////////////////////////////////////////////////////////

int MutrgProcessHit::FillHit(Event *evt){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"FillHit");
  return -1;
}

//////////////////////////////////////////////////////////////////

int MutrgProcessHit::Associate(TMutHitMap *mut_hitmap){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"Associate");
  return -1;
}

/////////////////////////////////////////////////////////////////
