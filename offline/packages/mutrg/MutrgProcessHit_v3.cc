#include "MutrgProcessHit_v3.hh"

#include <vector>
#include <TMatrix.h>

#include "getClass.h"
#include "Event.h"
#include "EventTypes.h"
#include "TMutGeo.h"
#include "TMutHitMap.h"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHeader.hh"
#include "MutrgHeaderArray_v1.hh"
#include "MutrgHit_v1.hh"
#include "MutrgHitArray_v1.hh"
#include "MutrgUnpack.hh"
#include "MutrgDecode.hh"

using namespace std;

///////////////////////////////////////////////////////////////////

MutrgProcessHit_v3::MutrgProcessHit_v3(bool init_flag) :
  MutrgProcessHit(false){
  class_name="MutrgProcessHit_v3";
  if(init_flag){CreateObject();} // MutrgProcessHit_v3::CreateObject
  mutrg_unpack=new MutrgUnpack();
  mutrg_decode=new MutrgDecode();
}

///////////////////////////////////////////////////////////////////

MutrgProcessHit_v3::~MutrgProcessHit_v3(void){
  // commented out by suggestion from Chris (2009Apr12)
  //if(mutrg_hits){delete mutrg_hits;}
  //if(mutrg_headers){delete mutrg_headers;}
  if(mutrg_unpack){delete mutrg_unpack;}
  if(mutrg_decode){delete mutrg_decode;}
  //mutrg_hits=NULL;
  //mutrg_headers=NULL;
  mutrg_unpack=NULL;
  mutrg_decode=NULL;
}

////////////////////////////////////////////////////////////////////

void MutrgProcessHit_v3::CreateObject(void){
  if(!mutrg_hits){mutrg_hits=new MutrgHitArray_v1("MutrgHit_v3");}
  if(!mutrg_headers){mutrg_headers=new MutrgHeaderArray_v1("MutrgHeader_v1");}
  return;
}

/////////////////////////////////////////////////////////////////

int MutrgProcessHit_v3::Init(PHCompositeNode *node,bool flag_reg){
  if(node && flag_reg){
    if(!RegMutrgHitArray(node,"MutrgHitArray")){
      printf("Error - %s::Init : ",ClassName());
      printf("Couldn't register MutrgHitArray.\n");
      return -1;
    }

    if(!RegMutrgHeaderArray(node,"MutrgHeaderArray")){
      printf("Error - %s::Init : ",ClassName());
      printf("Couldn't register MutrgHeaderArray.\n");
      return -1;
    }
  }    

  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgProcessHit_v3::InitRun(PHCompositeNode *node,bool flag_reg){
  if(node && flag_reg){
    if(!RegMutrgHitArray(node,"MutrgHitArray","MutrgHitArrayOld")){
      printf("Error - %s::InitRun : ",ClassName());
      printf("Couldn't register MutrgHitArray.\n");
      return -1;
    }

    if(!RegMutrgHeaderArray(node,"MutrgHeaderArray","MutrgHeaderArrayOld")){
      printf("Error - %s::InitRun : ",ClassName());
      printf("Couldn't register MutrgHeaderArray.\n");
      return -1;
    }
  }

  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgProcessHit_v3::ProcessEvent(PHCompositeNode *node){
  if(MutrgProcessHit::FillHit(node)){return -1;}

  TMutHitMap *mut_hitmap=findNode::getClass<TMutHitMap>(node,"TMutHitMap");
  if(mut_hitmap){
    if(Associate(mut_hitmap)){return -1;}
  }

  return 0;
}

////////////////////////////////////////////////////////////////

int MutrgProcessHit_v3::FillHit(Event *evt){
  mutrg_hits->Reset();
  mutrg_headers->Reset();

  unsigned int evt_type=evt->getEvtType();
  if(evt_type!=DATAEVENT){return 0;}

  for(int ip=0; ip<MutrgPar::NPACKET_ID; ip++){
    Packet *packet=evt->getPacket(MutrgPar::PACKET_ID[ip]);
    if(!packet){continue;}

    mutrg_unpack->Reset();
    mutrg_decode->Reset();

    // expand packet
    if(mutrg_unpack->Unpack(packet)){delete packet; continue;}

    // packet is not needed anymore
    delete packet;
    packet=NULL;

    // fill header from DCM
    MutrgHeader *mutrg_header=mutrg_headers->Insert();
    mutrg_header->SetPacketID(MutrgPar::PACKET_ID[ip]);
    if(mutrg_unpack->FillHeader(mutrg_header)){
      mutrg_headers->Remove(mutrg_header);
      continue;
    }

    // get data array from MRG
    vector<unsigned int> data_array=mutrg_unpack->GetData();

    // fill header from MRG
    mutrg_decode->SetData(data_array);
    if(mutrg_decode->FillHeader(mutrg_header)){continue;}

    // decode data, mutrg hit
    mutrg_decode->SetMutrgHeader(mutrg_header);
    if(mutrg_decode->Decode(mutrg_hits)){continue;}
  }

  return 0;
}

//////////////////////////////////////////////////////////////////

int MutrgProcessHit_v3::Associate(TMutHitMap *mut_hitmap){
  TMutHitMap::iterator mut_itr=mut_hitmap->range();

  unsigned int key_hoct=0x1;
  unsigned int key_cath=0x1;
  MutrgHitArray::const_private_itr mutrg_itr_beg;
  MutrgHitArray::const_private_itr mutrg_itr_end;
  MutrgHitArray::const_private_itr mutrg_itr;

  while(TMutHitMap::pointer mut_ptr=mut_itr.next()){
    TMutHit *mut_hit=mut_ptr->get();
    int arm=mut_hit->get_arm();
    int st=mut_hit->get_station();
    int oct=mut_hit->get_octant();
    int hoct=mut_hit->get_half_octant();
    int gap=mut_hit->get_gap();
    int cath=mut_hit->get_cathode();
    int strip=mut_hit->get_strip();

    // skip cathode without MuTRG
    if(cath!=MutrgPar::INSTALL_CATHODE[st]){continue;}

    // whether hit move to next hoct or not
    if(key_hoct!=MutrgKey::LocToKey(arm,st,oct,hoct,0,0,0)){
      MutrgHitArray::const_private_itr_pair mutrg_itr_pair=
	mutrg_hits->Range(arm,st,oct,hoct);
      mutrg_itr_beg=mutrg_itr_pair.first;
      mutrg_itr_end=mutrg_itr_pair.second;
    }

    // whether hit move to next gap or not
    if(key_cath!=MutrgKey::LocToKey(arm,st,oct,hoct,gap,cath,0)){
      mutrg_itr=mutrg_itr_beg;
    }

    // search matching between MuTr and MuTRG
    unsigned int key_mut=MutrgKey::LocToKey(arm,st,oct,hoct,strip);
    for(; mutrg_itr!=mutrg_itr_end; mutrg_itr++){
      unsigned int key_mutrg=mutrg_itr->first;
      if(key_mut==key_mutrg){
	MutrgHit *mhit=mutrg_itr->second;
	float q=mut_hit->get_q();
	if(q>mhit->GetMutMaxCharge()){mhit->SetMutMaxCharge(q);}
      }
      else if(key_mut>key_mutrg){continue;} // look at next mutrg_hit
      else if(key_mut<key_mutrg){break;}    // look at next mut_hit
    }
  }

  return 0;
}

/////////////////////////////////////////////////////////////////