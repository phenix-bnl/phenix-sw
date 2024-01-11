#include "MutrgFindTrk_v1.hh"

#include <vector>

#include "getClass.h"
#include "recoConsts.h"
#include "TMutTrkMap.h"
#include "TMutCoordMap.h"
#include "TMutClusMap.h"
#include "TMutHitMap.h"
#include "TMutHit.hh"
#include "MutrgKey.hh"
#include "MutrgTrk_v1.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray.hh"
#include "MutrgTrkArray_v1.hh"
#include "MutrgTrkMapping_v1.hh"

using namespace std;

//////////////////////////////////////////////////////////

MutrgFindTrk_v1::MutrgFindTrk_v1(bool init_flag) :
  MutrgFindTrk(false){
  class_name="MutrgFindTrk_v1";
  flag_set_mapping=false;
  mutrg_trks=NULL;
  mutrg_mapping=NULL;
  if(init_flag){CreateObject();} // MutrgFindTrk::CreateObject
}

///////////////////////////////////////////////////////////

MutrgFindTrk_v1::~MutrgFindTrk_v1(void){
  if(mutrg_mapping){delete mutrg_mapping; mutrg_mapping=NULL;}
}

//////////////////////////////////////////////////////////

void MutrgFindTrk_v1::CreateObject(void){
  if(!mutrg_trks){mutrg_trks=new MutrgTrkArray_v1("MutrgTrk_v1");}
  if(!mutrg_mapping){mutrg_mapping=new MutrgTrkMapping_v1();}
}

///////////////////////////////////////////////////////////

int MutrgFindTrk_v1::SetMapFile(const char *filename){
  int ret=mutrg_mapping->SetMapFile(filename);
  if(!ret){flag_set_mapping=true;}
  return ret;
}

/////////////////////////////////////////////////////////

int MutrgFindTrk_v1::SetMapDB(int run,MutrgPar::TrkDBVersion ver){
  int ret=mutrg_mapping->SetMapDB(run,ver);
  if(!ret){flag_set_mapping=true;}
  return ret;
}

///////////////////////////////////////////////////////////

int MutrgFindTrk_v1::SetMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver){
  int ret=mutrg_mapping->SetMapDB(ts,ver);
  if(!ret){flag_set_mapping=true;}
  return ret;
}

///////////////////////////////////////////////////////////

int MutrgFindTrk_v1::Init(PHCompositeNode *node,bool flag_reg){
  // There sould be no Mutrg object in node with PRDF input
  // if "MutrgTrkArray" is in dst_node, abort run
  if(node && flag_reg){
    if(!RegMutrgTrkArray(node,"MutrgTrkArray")){
      printf("Error - %s::Init : ",ClassName());
      printf("Couldn't register MutrgTrkArray.\n");
      return -1;
    }
  }    

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgFindTrk_v1::InitRun(PHCompositeNode *node,bool flag_reg){
  if(node && flag_reg){
    if(!RegMutrgTrkArray(node,"MutrgTrkArray","MutrgTrkArrayOld")){
      printf("Error - %s::InitRun : ",ClassName());
      printf("Couldn't register MutrgTrkArray.\n");
      return -1;
    }
  }

  if(!flag_set_mapping){
    recoConsts *rc=recoConsts::instance();
    if(!rc){
      printf("Error - %s::InitRun : No recoConsts\n",ClassName());
      return -1;
    }

    int run=rc->get_IntFlag("RUNNUMBER");

    if(SetMapDB(run)){return -1;}
  }

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgFindTrk_v1::FindTrk(MutrgHitArray *mutrg_hits){
  if(mutrg_hits->Readback()){return -1;}

  mutrg_trks->Reset();

  unsigned short index=0; // index for MuTRG Track
  for(int iarm=0; iarm<MutrgPar::NARM; iarm++){
    // track finding starts from station=2
    MutrgHitArray::const_private_itr_pair itr_pair=mutrg_hits->Range(iarm,2);
    MutrgHitArray::const_private_itr mutrg_itr_beg=itr_pair.first;
    MutrgHitArray::const_private_itr mutrg_itr_end=itr_pair.second;

    for(MutrgHitArray::const_private_itr mutrg_itr=mutrg_itr_beg;
	mutrg_itr!=mutrg_itr_end; mutrg_itr++){
      unsigned int key=mutrg_itr->first;
      MutrgHit *mutrg_hit=mutrg_itr->second;

      int arm,st,oct,hoct,strip;
      mutrg_hit->GetLocation(arm,st,oct,hoct,strip);

      int nstrip_hoct0[MutrgPar::NSTATION]={
	MutrgPar::NSTRIP_IN_HALFOCTANT(arm,0,oct,0),
	MutrgPar::NSTRIP_IN_HALFOCTANT(arm,1,oct,0),
	MutrgPar::NSTRIP_IN_HALFOCTANT(arm,2,oct,0)
      };

      int stripo=strip+hoct*nstrip_hoct0[st];

      vector<unsigned short> stripos_st[2]; // st1,st2
      mutrg_mapping->FindFromSt2(arm,oct,stripo,stripos_st[0],stripos_st[1]);

      unsigned int ncand=stripos_st[0].size();
      for(unsigned int ic=0; ic<ncand; ic++){
	bool flag_accept=true;
	unsigned int key1[MutrgPar::NSTATION]={0,0,key};

	for(int ist=0; ist<2; ist++){
	  int strip1=stripos_st[ist][ic]%nstrip_hoct0[ist];
	  key1[ist]=MutrgKey::LocToKey(arm,ist,oct,hoct,strip1);

	  MutrgHit *hit=mutrg_hits->Find(key1[ist]);
	  if(!hit){flag_accept=false; break;}
	}

	if(!flag_accept){continue;}

	MutrgTrk *trk=mutrg_trks->Insert();
	for(int ist=0; ist<MutrgPar::NSTATION; ist++){
	  trk->SetHit(ist,key1[ist]);
	}
	trk->SetIndex(index);
	index++;
      }
    }
  }

  return 0;
}

//////////////////////////////////////////////////////////

int MutrgFindTrk_v1::Associate(TMutTrkMap *mut_trkmap){
  TMutTrkMap::iterator itr_trk=mut_trkmap->range();
  while(1){
    TMutTrkMap::pointer ptr_trk=itr_trk.next();
    if(!ptr_trk){break;}
    TMutTrk *mut_trk=ptr_trk->get();

    int arm=mut_trk->get_arm();
    int oct=mut_trk->get_octant();

    vector<TMutHit*> mut_hits[MutrgPar::NSTATION];

    TMutCoordMap::key_iterator kit_coord=
      mut_trk->get_associated<TMutCoord>();
    while(1){
      TMutCoordMap::pointer ptr_coord=kit_coord.next();
      if(!ptr_coord){break;}
      TMutCoord *mut_coord=ptr_coord->get();

      TMutClusMap::key_iterator kit_clus=
	mut_coord->get_associated<TMutClus>();
      while(1){
	TMutClusMap::pointer ptr_clus=kit_clus.next();
	if(!ptr_clus){break;}
	TMutClus *mut_clus=ptr_clus->get();

	TMutHitMap::key_iterator kit_hit=
	  mut_clus->get_associated<TMutHit>();
	while(1){
	  TMutHitMap::pointer ptr_hit=kit_hit.next();
	  if(!ptr_hit){break;}
	  TMutHit *mut_hit=ptr_hit->get();

	  int st=mut_hit->get_station();
	  int gap=mut_hit->get_gap();
	  int cath=mut_hit->get_cathode();
	  if(!MutrgPar::IS_INSTALL_GAP[st][gap] ||
	     cath!=MutrgPar::INSTALL_CATHODE[st]){continue;}

	  mut_hits[st].push_back(mut_hit);
	}
      }
    }

    vector<MutrgTrk*> mutrg_trks_acpt;
    vector<MutrgTrk*> mutrg_trks_cddt;

    int ntrk=mutrg_trks->GetSize();
    for(int itrk=0; itrk<ntrk; itrk++){
      MutrgTrk *trk=mutrg_trks->Get(itrk);
      unsigned int key0=trk->GetHit(0);

      if(arm!=MutrgKey::KeyToArm(key0) ||
	 oct!=MutrgKey::KeyToOctant(key0)){continue;}

      mutrg_trks_acpt.push_back(trk);
    }

    for(int ist=0; ist<MutrgPar::NSTATION; ist++){
      mutrg_trks_cddt=mutrg_trks_acpt;
      mutrg_trks_acpt.clear();

      for(unsigned int itrk=0; itrk<mutrg_trks_cddt.size(); itrk++){
	unsigned int mutrg_key=mutrg_trks_cddt[itrk]->GetHit(ist);
	int mutrg_hoct=MutrgKey::KeyToHalfOctant(mutrg_key);
	int mutrg_strip=MutrgKey::KeyToStrip(mutrg_key);

	for(unsigned int ihit=0; ihit<mut_hits[ist].size(); ihit++){
	  int mut_hoct=mut_hits[ist][ihit]->get_half_octant();
	  int mut_strip=mut_hits[ist][ihit]->get_strip();
	  if(mutrg_hoct==mut_hoct && mutrg_strip==mut_strip){
	    mutrg_trks_acpt.push_back(mutrg_trks_cddt[itrk]);
	    break;
	  }
	}
      }
    }

    unsigned short index=mut_trk->get_index();
    unsigned int uid=mut_trk->get_key().get_obj_key();
    for(unsigned int itrk=0; itrk<mutrg_trks_acpt.size(); itrk++){
      mutrg_trks_acpt[itrk]->AddMuTrIndex(index);
      mutrg_trks_acpt[itrk]->AddMuTrUid(uid);
    }
  }

  return 0;
}

//////////////////////////////////////////////////////////
