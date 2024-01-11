#include "MutrgProcessHit_v2.hh"

#include <vector>
#include <TMatrix.h>

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

MutrgProcessHit_v2::MutrgProcessHit_v2(bool init_flag) :
  MutrgProcessHit(false){
  class_name="MutrgProcessHit_v2";
  if(init_flag){CreateObject();} // MutrgProcessHit_v2::CreateObject
  mutrg_unpack=new MutrgUnpack();
  mutrg_decode=new MutrgDecode();
}

///////////////////////////////////////////////////////////////////

MutrgProcessHit_v2::~MutrgProcessHit_v2(void){
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

void MutrgProcessHit_v2::CreateObject(void){
  if(!mutrg_hits){mutrg_hits=new MutrgHitArray_v1("MutrgHit_v2");}
  if(!mutrg_headers){mutrg_headers=new MutrgHeaderArray_v1("MutrgHeader_v1");}
  return;
}

/////////////////////////////////////////////////////////////////

int MutrgProcessHit_v2::Init(PHCompositeNode *node,bool flag_reg){
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

int MutrgProcessHit_v2::InitRun(PHCompositeNode *node,bool flag_reg){
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

int MutrgProcessHit_v2::ProcessEvent(PHCompositeNode *node){
  if(MutrgProcessHit::FillHit(node)){return -1;}
  return 0;
}

////////////////////////////////////////////////////////////////

int MutrgProcessHit_v2::FillHit(Event *evt){
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
