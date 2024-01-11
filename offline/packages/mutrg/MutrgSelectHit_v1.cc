#include "MutrgSelectHit_v1.hh"

#include <vector>

#include "getClass.h"
#include "MutrgKey.hh"
#include "MutrgDataProcessUtil.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray_v1.hh"

using namespace std;

////////////////////////////////////////////////////////////////////

MutrgSelectHit_v1::MutrgSelectHit_v1(bool init_flag) :
  MutrgSelectHit(false){
  class_name="MutrgSelectHit_v1";
  mutrg_hits_sel=NULL;
  if(init_flag){CreateObject();} // MutrgSelectHit_v1::CreateObject
}

////////////////////////////////////////////////////////////////////

MutrgSelectHit_v1::~MutrgSelectHit_v1(void){
}

///////////////////////////////////////////////////////////////////

void MutrgSelectHit_v1::CreateObject(void){
  if(!mutrg_hits_sel){mutrg_hits_sel=new MutrgHitArray_v1("MutrgHit_v2");}
  return;
}

///////////////////////////////////////////////////////////

int MutrgSelectHit_v1::Init(PHCompositeNode *node,bool flag_reg){
  if(node && flag_reg){
    if(!RegMutrgSelHitArray(node,"MutrgSelHitArray")){
      printf("Error - %s::Init : ",ClassName());
      printf("Couldn't register MutrgSelHitArray.\n");
      return -1;
    }
  }    

  return 0;
}

////////////////////////////////////////////////////////////////

int MutrgSelectHit_v1::InitRun(PHCompositeNode *node,bool flag_reg){
  if(node && flag_reg){
    if(!RegMutrgSelHitArray(node,"MutrgSelHitArray","MutrgSelHitArrayOld")){
      printf("Error - %s::InitRun : ",ClassName());
      printf("Couldn't register MutrgSelHitArray.\n");
      return -1;
    }
  }

  PrintParameters();

  return 0;
}

//////////////////////////////////////////////////////////////////////

int MutrgSelectHit_v1::ProcessEvent(PHCompositeNode *node){
  MutrgHitArray *mutrg_hits=
    findNode::getClass<MutrgHitArray>(node,"MutrgHitArray");
  if(!mutrg_hits){
    printf("Error - %s::ProcessEvent : Couldn't find MutrgHitArray\n",
	   ClassName());
    return -1;
  }

  if(mutrg_hits->Readback()){return -1;}

  mutrg_hits_sel->Set(mutrg_hits);

  if(hit_clk_ext!=0){ // extend hit clock of each MutrgHit
    mutrg_hits_sel->ExtendHitClock(hit_clk_ext);
  }

  if(hit_clk_shift!=0){ // shift hit clock of each MutrgHit
    mutrg_hits_sel->ShiftHitClock(hit_clk_shift);
  }

  if(do_multiplicity_cut){ // multiplicity cut
    MultiplicityCut();
  }

  if(do_clsize_cut){ // cluster size cut
    ClusterSizeCut();
  }

  if(do_clustering && max_cluster_size>0){ // clustering
    MutrgDataProcessUtil::Clustering(mutrg_hits_sel,max_cluster_size);
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////

int MutrgSelectHit_v1::MultiplicityCut(void){
  map<unsigned int,vector<int> > nhit_map;
  MutrgDataProcessUtil::CountMultiplicity(mutrg_hits_sel,nhit_map);

  MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits_sel->Range();
  MutrgHitArray::const_private_itr itr_beg=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;

  vector<int> nhit(MutrgPar::MAX_NHITCLOCK,0);
  unsigned int mask_oct=MutrgKey::Mask(true,true,true,false,false);
  unsigned int key_oct_pre=0xffffffff;
  for(MutrgHitArray::const_private_itr itr=itr_beg; itr!=itr_end; itr++){
    unsigned int key=itr->first;
    unsigned int key_oct=(key&mask_oct);
    MutrgHit *hit=itr->second;

    int arm=MutrgKey::KeyToArm(hit->GetKey());
    int st=MutrgKey::KeyToStation(hit->GetKey());

    if(key_oct!=key_oct_pre){
      map<unsigned int,vector<int> >::iterator itr_nhit=nhit_map.find(key_oct);
      if(itr_nhit!=nhit_map.end()){nhit=itr_nhit->second;}
      else{
	printf("Warning - %s::MultiplicityCut : ",ClassName());
	printf("Couldn't find nhit for key_oct = %8.8x\n",key_oct);
	nhit.resize(MutrgPar::MAX_NHITCLOCK,0);
      }

      key_oct_pre=key_oct;
    }

    for(unsigned int iclk=0; iclk<nhit.size(); iclk++){
      if(nhit[iclk]<multiplicity_threshold[arm][st]){continue;}

      unsigned int mask=(~(0x1<<iclk));
      unsigned int hit_clk=hit->GetHitClock();
      hit->SetHitClock(hit_clk&mask);
    }
  }
  
  return 0;
}

//////////////////////////////////////////////////////////////////

int MutrgSelectHit_v1::ClusterSizeCut(void){
  for(int iclk=0; iclk<MutrgPar::MAX_NHITCLOCK; iclk++){
    unsigned int mask=(0x1<<iclk);
    vector<vector<MutrgHit*> > clusters;
    MutrgDataProcessUtil::Clustering(mutrg_hits_sel,mask,clusters);

    for(unsigned int icl=0; icl<clusters.size(); icl++){
      int clsize=clusters[icl].size();

      //unsigned int hit_clk1=clusters[icl][0]->GetHitClock();
      //if(clsize==1 && (hit_clk1&mask)==0){clsize=0;}

      int arm,st,oct,hoct,strip;
      clusters[icl][0]->GetLocation(arm,st,oct,hoct,strip);

      if(clsize<clsize_threshold[arm][st]){continue;}

      for(unsigned int ihit=0; ihit<clusters[icl].size(); ihit++){
	unsigned int hit_clk=clusters[icl][ihit]->GetHitClock();
	clusters[icl][ihit]->SetHitClock(hit_clk&(~mask));
      }
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////
