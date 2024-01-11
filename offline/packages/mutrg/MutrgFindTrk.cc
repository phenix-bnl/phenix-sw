#include "MutrgFindTrk.hh"

#include "TMutTrkMap.h"
#include "MutrgDataProcessUtil.hh"
#include "MutrgHitArray.hh"
#include "MutrgTrkArray.hh"

using namespace std;

/////////////////////////////////////////////////////////////////

MutrgFindTrk::MutrgFindTrk(bool init_flag){
  class_name="MutrgFindTrk";
  mutrg_trks=NULL;
  if(init_flag){CreateObject();} // MutrgFindTrk::CreateObject
  mutrg_hits_name="MutrgSelHitArray";
  allow_mutr_strip_diff=1;
}

//////////////////////////////////////////////////////////////////

MutrgFindTrk::~MutrgFindTrk(void){
  // commented out by suggestion from Chris (2009Apr12)
  //if(mutrg_trks){delete mutrg_trks; mutrg_trks=NULL;}
}

///////////////////////////////////////////////////////////////////

void MutrgFindTrk::CreateObject(void){
  if(!mutrg_trks){mutrg_trks=new MutrgTrkArray("MutrgTrk");}
  return;
}

///////////////////////////////////////////////////////////////////

int MutrgFindTrk::SetMapFile(const char *filename){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"SetMapFile");
  return -1;
}

//////////////////////////////////////////////////////////////////

int MutrgFindTrk::SetMapDB(int run,MutrgPar::TrkDBVersion ver){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"SetMapDB");
  return -1;
}

//////////////////////////////////////////////////////////////////

int MutrgFindTrk::SetMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"SetMapDB");
  return -1;
}

//////////////////////////////////////////////////////////////////

int MutrgFindTrk::SetMutrgTrkArray(MutrgTrkArray *trks,bool flag_delete){
  return MutrgDataProcessUtil::SetMutrgObject(mutrg_trks,trks,flag_delete);
}

////////////////////////////////////////////////////////////////////

MutrgTrkArray*
MutrgFindTrk::RegMutrgTrkArray(PHCompositeNode *node,
			       const char *name,const char *rename){
  return MutrgDataProcessUtil::RegMutrgObject(mutrg_trks,node,name,rename);
}

//////////////////////////////////////////////////////////////////

int MutrgFindTrk::Init(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"Init");
  return 0;
}

///////////////////////////////////////////////////////////////////

int MutrgFindTrk::InitRun(PHCompositeNode *node,bool flag_reg){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"InitRun");
  return 0;
}

///////////////////////////////////////////////////////////////////

int MutrgFindTrk::ProcessEvent(PHCompositeNode *node){
  if(FindTrk(node)){return -1;}

  TMutTrkMap *mut_trkmap=findNode::getClass<TMutTrkMap>(node,"TMutTrkMap");
  if(mut_trkmap){
    if(Associate(mut_trkmap)){return -1;}
  }

  return 0;
}

///////////////////////////////////////////////////////////////////

int MutrgFindTrk::FindTrk(PHCompositeNode *node){
  MutrgHitArray *mutrg_hits=
    findNode::getClass<MutrgHitArray>(node,"MutrgHitArray");
  if(!mutrg_hits){
    printf("Error - %s::FindTrk : Coundn't find MutrgHitArray.\n",ClassName());
    return -1;
  }

  return FindTrk(mutrg_hits);
}

///////////////////////////////////////////////////////////////////

int MutrgFindTrk::FindTrk(MutrgHitArray *mutrg_hits){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"FindTrk");
  return -1;
}

//////////////////////////////////////////////////////////////////

int MutrgFindTrk::Associate(TMutTrkMap *mut_trkmap){
  MutrgDataProcessUtil::WarnDefFuncCall(ClassName(),"Associate");
  return -1;
}

//////////////////////////////////////////////////////////////////
