#include "MutrgReco.hh"

#include "Event.h"
#include "getClass.h"
#include "TMutHitMap.h"
#include "Fun4AllReturnCodes.h"
#include "MutrgPar.hh"
#include "MutrgProcessHit_v2.hh"
#include "MutrgSelectHit_v1.hh"
#include "MutrgFindTrk_v2.hh"
#include "MutrgTrkArray.hh"

////////////////////////////////////////////////////////////

MutrgReco::MutrgReco(const char *name) : SubsysReco(name){
  input_mode=INPUT_PRDF;
  mutrg_prohit=NULL;
  mutrg_selhit=NULL;
  mutrg_findtrk=NULL;
  flag_prohit=true;
  flag_selhit=true;
  flag_findtrk=true;
  CreateModule();
}

//////////////////////////////////////////////////////////

MutrgReco::~MutrgReco(void){
  if(mutrg_prohit){delete mutrg_prohit; mutrg_prohit=NULL;}
  if(mutrg_selhit){delete mutrg_selhit; mutrg_selhit=NULL;}
  if(mutrg_findtrk){delete mutrg_findtrk; mutrg_findtrk=NULL;}
}

///////////////////////////////////////////////////////////

int MutrgReco::Init(PHCompositeNode *top_node){
  if(!flag_prohit && !flag_selhit && !flag_findtrk){
    printf("%s::Init : flag_prohit=%d\n",ClassName(),flag_prohit);
    printf("%s::Init : flag_selhit=%d\n",ClassName(),flag_selhit);
    printf("%s::Init : flag_findtrk=%d\n",ClassName(),flag_findtrk);
    printf("%s::Init : Nothing to do\n",ClassName());
    return DONOTREGISTERSUBSYSTEM;
  }

  PHNodeIterator it(top_node);
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(it.findFirst("PHCompositeNode","DST"));

  if(!dst_node){
    printf("Error - %s::Init : No DST node. Do nothing.\n",ClassName());
    return DONOTREGISTERSUBSYSTEM;
  }

  bool flag_reg=true;
  switch(input_mode){
  case INPUT_PRDF: flag_reg=true;  break;
  case INPUT_DST : flag_reg=false; break;
  default:
    printf("Error - %s::Init : Unknown input mode.\n",ClassName());
    return DONOTREGISTERSUBSYSTEM;
  }

  if(flag_prohit){
    int ret=mutrg_prohit->Init(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::Init : %s::Init failed\n",
	     ClassName(),mutrg_prohit->ClassName());
      DoProcessAll(false);
      return DONOTREGISTERSUBSYSTEM;
    }
  }

  if(flag_selhit){
    int ret=mutrg_selhit->Init(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::Init : %s::Init failed\n",
	     ClassName(),mutrg_selhit->ClassName());
      DoProcessAll(false);
      return DONOTREGISTERSUBSYSTEM;
    }
  }

  if(flag_findtrk){
    int ret=mutrg_findtrk->Init(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::Init : %s::Init failed\n",
	     ClassName(),mutrg_findtrk->ClassName());
      DoProcessAll(false);
      return DONOTREGISTERSUBSYSTEM;
    }
  }

  return 0;
}

/////////////////////////////////////////////////////////////

int MutrgReco::InitRun(PHCompositeNode *top_node){
  // just to store data from DB
  MutrgPar::NSTRIP_IN_OCTANT(0,0,0);

  PHNodeIterator it(top_node);
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(it.findFirst("PHCompositeNode","DST"));

  bool flag_reg=true;
  switch(input_mode){
  case INPUT_PRDF: flag_reg=false; break;
  case INPUT_DST : flag_reg=true;  break;
  default:
    printf("Error - %s::Init : Unknown input mode.\n",ClassName());
    return ABORTRUN;
  }

  if(flag_prohit){
    int ret=mutrg_prohit->InitRun(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::InitRun : %s::InitRun failed\n",
	     ClassName(),mutrg_prohit->ClassName());
      DoProcessAll(false);
      return ABORTRUN;
    }
  }

  if(flag_selhit){
    int ret=mutrg_selhit->InitRun(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::InitRun : %s::InitRun failed\n",
	     ClassName(),mutrg_selhit->ClassName());
      DoProcessAll(false);
      return ABORTRUN;
    }
  }

  if(flag_findtrk){
    int ret=mutrg_findtrk->InitRun(dst_node,flag_reg);

    if(ret){
      printf("Error - %s::InitRun : %s::InitRun failed\n",
	     ClassName(),mutrg_findtrk->ClassName());
      DoProcessAll(false);
      return ABORTRUN;
    }
  }

  return 0;
}

//////////////////////////////////////////////////////////////

int MutrgReco::End(PHCompositeNode *top_node){
  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgReco::process_event(PHCompositeNode *top_node){
  if(flag_prohit){
    if(mutrg_prohit->ProcessEvent(top_node)){return ABORTRUN;}
  }

  if(flag_selhit){
    if(mutrg_selhit->ProcessEvent(top_node)){return ABORTRUN;}
  }

  if(flag_findtrk){
    if(mutrg_findtrk->ProcessEvent(top_node)){return ABORTRUN;}
  }

  return EVENT_OK;
}

////////////////////////////////////////////////////////////////

void MutrgReco::CreateModule(void){
  if(!mutrg_prohit){mutrg_prohit=new MutrgProcessHit_v2();} // ~run10
  if(!mutrg_selhit){mutrg_selhit=new MutrgSelectHit_v1();} // ~run11
  if(!mutrg_findtrk){mutrg_findtrk=new MutrgFindTrk_v2();} // ~run11
}

////////////////////////////////////////////////////////////////

int MutrgReco::SetTrkMapFile(const char *filename){
  return mutrg_findtrk->SetMapFile(filename);
}

/////////////////////////////////////////////////////////////////

int MutrgReco::SetTrkMapDB(int run,MutrgPar::TrkDBVersion ver){
  return mutrg_findtrk->SetMapDB(run,ver);
}

/////////////////////////////////////////////////////////////////

int MutrgReco::SetTrkMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver){
  return mutrg_findtrk->SetMapDB(ts,ver);
}

/////////////////////////////////////////////////////////////////

void MutrgReco::SetHitNameForTrk(const char *name){
  mutrg_findtrk->SetHitNameForTrk(name);
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgReco::SetAllowMutrStripDiff(int diff){
  mutrg_findtrk->SetAllowMutrStripDiff(diff);
  return;
}

/////////////////////////////////////////////////////////////////

void MutrgReco::DoProcessAll(bool flag){
  DoProcessHit(flag);
  DoSelectHit(flag);
  DoFindTrk(flag);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgReco::DoExtendHitClock(int clk_ext,int clk_shift){
  mutrg_selhit->DoExtendHitClock(clk_ext,clk_shift);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgReco::DoMultiplicityCut(int threshold){
  mutrg_selhit->DoMultiplicityCut(threshold);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgReco::DoMultiplicityCut(int thre_s0,int thre_s1,int thre_s2,
				  int thre_n0,int thre_n1,int thre_n2){
  mutrg_selhit->DoMultiplicityCut(thre_s0,thre_s1,thre_s2,
				  thre_n0,thre_n1,thre_n2);
  return;
 }

///////////////////////////////////////////////////////////////////

void MutrgReco::DoClusterSizeCut(int threshold){
  mutrg_selhit->DoClusterSizeCut(threshold);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgReco::DoClusterSizeCut(int thre_s0,int thre_s1,int thre_s2,
				 int thre_n0,int thre_n1,int thre_n2){
  mutrg_selhit->DoClusterSizeCut(thre_s0,thre_s1,thre_s2,
				 thre_n0,thre_n1,thre_n2);
  return;
}

///////////////////////////////////////////////////////////////////

void MutrgReco::DoClustering(bool flag,int max_size){
  mutrg_selhit->DoClustering(flag,max_size);
  return;
}

///////////////////////////////////////////////////////////////////
