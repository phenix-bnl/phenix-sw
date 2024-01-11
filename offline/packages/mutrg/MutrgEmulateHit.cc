#include "MutrgEmulateHit.hh"

#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "getClass.h"
#include "TMutHitMap.h"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray_v1.hh"

////////////////////////////////////////////////////////////////

MutrgEmulateHit::MutrgEmulateHit(void){
  class_name="MutrgEmulateHit";
  mutrg_hits=NULL;
}

/////////////////////////////////////////////////////////////////

int MutrgEmulateHit::Init(PHCompositeNode *top_node){
  PHNodeIterator it(top_node);
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(it.findFirst("PHCompositeNode","DST"));

  if(!dst_node){
    printf("Error - %s::Init : No DST node. Do nothing.\n",ClassName());
    return -1;
  }

  // Add MutrgHitArray to DST node
  MutrgHitArray *mutrg_hits1=new MutrgHitArray_v1("MutrgHit_v2");
  PHIODataNode<MutrgHitArray> *mutrg_hits_node=
    new PHIODataNode<MutrgHitArray>(mutrg_hits1,"MutrgHitArray",
				    "PHObject");
  dst_node->addNode(mutrg_hits_node);

  return 0;
}

////////////////////////////////////////////////////////////////////

int MutrgEmulateHit::InitRun(PHCompositeNode *top_node){
  mutrg_hits=findNode::getClass<MutrgHitArray>(top_node,"MutrgHitArray");
  if(!mutrg_hits){
    printf("Error - %s : No MutrgHitArray\n",ClassName());
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////////////////

int MutrgEmulateHit::ProcessEvent(PHCompositeNode *top_node){
  mutrg_hits->Reset();

  TMutHitMap *mut_hitmap=findNode::getClass<TMutHitMap>(top_node,"TMutHitMap");
  if(!mut_hitmap){return 0;}

  TMutHitMap::iterator itr_mut=mut_hitmap->range();
  while(TMutHitMap::pointer ptr_mut=itr_mut.next()){
    TMutHit *mut_hit=ptr_mut->get();
    int arm=mut_hit->get_arm();
    int st=mut_hit->get_station();
    int oct=mut_hit->get_octant();
    int hoct=mut_hit->get_half_octant();
    int gap=mut_hit->get_gap();
    int cath=mut_hit->get_cathode();
    int strip=mut_hit->get_strip();

    // skip gap/cathode without MuTRG
    if(cath!=MutrgPar::INSTALL_CATHODE[st] ||
       !MutrgPar::IS_INSTALL_GAP[st][gap]){continue;}

    unsigned int key=MutrgKey::LocToKey(arm,st,oct,hoct,strip);
    MutrgHit *hit=mutrg_hits->Insert(key);
    hit->SetState(0);
    hit->SetHitClock(0x4);
  }

  return 0;
}

////////////////////////////////////////////////////////////////////
