#include "MutrgUnpack_v1.hh"

#include <vector>

#include "Event.h"
#include "EventTypes.h"
#include "TMutGeo.h"
#include "TMutHitMap.h"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHit_v1.hh"
#include "MutrgHitArray_v1.hh"

using namespace std;

///////////////////////////////////////////////////////////////////

MutrgUnpack_v1::MutrgUnpack_v1(bool init_flag) :
  MutrgUnpack(false){
  if(init_flag){CreateObject();} // MutrgUnpack_v1::CreateObject
}

///////////////////////////////////////////////////////////////////

MutrgUnpack_v1::~MutrgUnpack_v1(void){
  if(mutrg_hit){delete mutrg_hit;}
  if(mutrg_array){delete mutrg_array;}
  if(mutrg_header){delete mutrg_header;}
  mutrg_hit=NULL;
  mutrg_array=NULL;
  mutrg_header=NULL;
}

////////////////////////////////////////////////////////////////////

void MutrgUnpack_v1::CreateObject(void){
  class_name="MutrgUnpack_v1";
  if(!mutrg_hit){mutrg_hit=new MutrgHit_v1();}
  if(!mutrg_array){mutrg_array=new MutrgHitArray_v1();}
  if(!mutrg_header){mutrg_header=new MutrgDataHeader();}
  return;
}

/////////////////////////////////////////////////////////////////

int MutrgUnpack_v1::Init(void){
  const int NSTRIP[3]={96,192,320};
  fpout=new TFile("output.root","recreate");
  for(int ist=0; ist<3; ist++){
    char hname[256];
    sprintf(hname,"hmrg_hit_st%d",ist);
    hmrg_hit[ist]=new TH2F(hname,hname,NSTRIP[ist],-0.5,NSTRIP[ist]-0.5,
			   7,-0.5,6.5);
  }

  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgUnpack_v1::End(void){
  fpout->cd();
  for(int ist=0; ist<3; ist++){hmrg_hit[ist]->Write();}
  fpout->Close();
  return 0;
}

/////////////////////////////////////////////////////////////////

int MutrgUnpack_v1::Unpack(Event *evt){
  const int PACKET_NUM=11500;
  const int MAX_DATA_LENGTH=1024;
  const int NSTATION=3;
  const int NDATA[NSTATION]={6,12,20}; // num of strip / 16
  const int NDATA_SUM=38;
  const int NSTRIP_IN_DATA=16;
  const int NHEADER=5;
  const int NFOOTER=2;

  printf("evtnum=%d\n",evt->getEvtSequence());

  mutrg_array->clear();

  unsigned int evtnum_prdf=evt->getEvtSequence();
  unsigned int evt_type=evt->getEvtType();
  if(evt_type!=DATAEVENT){return -1;}

  Packet *packet=evt->getPacket(PACKET_NUM);
  if(!packet){return -1;}

  int array[MAX_DATA_LENGTH],nget;
  int ret=packet->fillIntArray(array,MAX_DATA_LENGTH,&nget,"DATA");
  if(ret || nget!=packet->getDataLength()){
    printf("Error - MutrgUnpack_v1::Unpack : ");
    printf("Fail to get data array (ret=%d, nget=%d)\n",ret,nget);
    delete packet;
    return -1;
  }

  // find data begin
  int data_num_beg=-1;
  for(int id=0; id<nget; id++){
    unsigned int uarray=(unsigned int)array[id];
    if(((uarray>>16)&0xff00)==0x4000){data_num_beg=id; break;}
  }

  if(data_num_beg<0){
    printf("Error : Couldn't find data begin flag.\n");
    delete packet;
    return -1;
  }
  else if(data_num_beg!=0){
    printf("Warning : data_num_beg=%d\n",data_num_beg);
  }

  // find data end
  int data_num_end=-1;
  for(int id=nget-1; id>=0; id--){
    unsigned int uarray=(unsigned int)array[id];
    if(((uarray>>16)&0xff00)==0x8000){data_num_end=id; break;}
  }

  if(data_num_end<0){
    printf("Error : Couldn't find data end flag.\n");
    delete packet;
    return -1;
  }

  // divide data array to header, data, footer
  int *head=array+data_num_beg;
  int *data_org=array+data_num_beg+NHEADER;
  int *foot=array+nget-NFOOTER;
  int ndata=nget-data_num_beg-NHEADER-NFOOTER;
  if(ndata%(2*NDATA_SUM)!=0){
    printf("Error : data size is inconsistent.(ndata=%d)\n",ndata);
    return -1;
  }

  mutrg_header->event_nclk    =ndata/(2*NDATA_SUM);
  mutrg_header->evtnum_mutrg  =((unsigned short)head[0]&0xffff);
  mutrg_header->flag          =((unsigned short)head[1]&0xffff);
  mutrg_header->detector_id   =((unsigned short)head[2]&0xffff);
  mutrg_header->module_address=((unsigned short)head[3]&0xffff);
  mutrg_header->clock_num     =((unsigned short)head[4]&0xffff);
  mutrg_header->user_word     =((unsigned short)foot[0]&0xffff);
  mutrg_header->parity_word   =((unsigned short)foot[1]&0xffff);

  if(evtnum_prdf%0x10000!=mutrg_header->evtnum_mutrg){
    printf("Error : event number inconsistent.(PRDF:%d, MuTRG:%d)\n",
	   evtnum_prdf,mutrg_header->evtnum_mutrg);
  }

  // arrange data array
  vector<unsigned int> data[2][NDATA_SUM];
  for(int ioct=0; ioct<2; ioct++){
    for(int id=0; id<NDATA_SUM; id++){
      data[ioct][id].resize(mutrg_header->event_nclk);
    }
  }

  for(int id=0; id<ndata; id++){
    int evt_clk=id/(2*NDATA_SUM);
    int oct_num=(id/NDATA_SUM)%2;  // 0 or 1
    int data_num_oct=id%NDATA_SUM; // data number in octant
    data[oct_num][data_num_oct][evt_clk]=(unsigned int)data_org[id];
  }

  // fill mutrg_array
  for(int ioct=0; ioct<2; ioct++){
    if(ioct==1){continue;} // so far no data in ioct==1

    for(int id=0; id<NDATA_SUM; id++){
      // calculate station number
      int station=-1;
      int data_num_st=-1;
      int ndata_pre=0;
      for(int ist=0; ist<NSTATION; ist++){
	if(id>=ndata_pre && id<ndata_pre+NDATA[ist]){
	  station=ist;
	  data_num_st=id-ndata_pre;
	}
	ndata_pre+=NDATA[ist];
      }

      int arm=1; // temporary
      int oct=1; // temporary
      int pl=MutrgPar::INSTALL_PLANE[station];
      int nstrip_hoct0=TMutGeo::get_n_strip(arm,station,oct,0,0,pl);

      for(int istp=0; istp<NSTRIP_IN_DATA; istp++){
	MutrgHit *mhit=NULL;
	int stpo=data_num_st*NSTRIP_IN_DATA+istp;
	int hoct=(stpo/nstrip_hoct0 ? 1 : 0);
	int stp=stpo-hoct*nstrip_hoct0;

	for(int iclk=0; iclk<mutrg_header->event_nclk; iclk++){
	  if(!((data[ioct][id][iclk]>>istp)&0x1)){continue;}

	  if(!mhit){
	    mhit=mutrg_hit->Create();
	    mhit->SetLocation(arm,station,oct,hoct,stp);
	  }

	  mhit->SetHitClock(iclk,true);

	  hmrg_hit[station]->Fill(stpo,iclk);
	}

	if(mhit){mutrg_array->insert(mhit->GetKey(),mhit);}

	//if(mhit){
	//printf("MutrgHit : %4d : arm=%d st=%d strip=%d hit=%x (key=%x)\n",
	//evtnum_prdf,arm,station,stp,
	//mhit->GetHitClock(),mhit->GetKey());
	//}
      }
    }
  }

  return 0;
}

//////////////////////////////////////////////////////////////////

int MutrgUnpack_v1::Associate(TMutHitMap *mut_hitmap){
  TMutHitMap::iterator mut_itr=mut_hitmap->range();

  unsigned int key_hoct=0x1;
  unsigned int key_cath=0x1;
  MutrgHitArray::private_itr mutrg_itr_beg;
  MutrgHitArray::private_itr mutrg_itr_end;
  MutrgHitArray::private_itr mutrg_itr;

  while(1){
    TMutHitMap::pointer mut_ptr=mut_itr.next();
    if(!mut_ptr){break;}

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
      MutrgHitArray::private_itr_pair mutrg_itr_pair=
	mutrg_array->range(arm,st,oct,hoct);
      mutrg_itr_beg=mutrg_itr_pair.first;
      mutrg_itr_end=mutrg_itr_pair.second;
    }

    // whether hit move to next gap or not
    if(key_cath!=MutrgKey::LocToKey(arm,st,oct,hoct,gap,cath,0)){
      mutrg_itr=mutrg_itr_beg;
    }

    // search matching between MuTr and MuTRG
    unsigned int key_mut=MutrgKey::LocToKey(arm,st,oct,hoct,0,0,strip);
    for(; mutrg_itr!=mutrg_itr_end; mutrg_itr++){
      unsigned int key_mutrg=mutrg_itr->first;
      if(key_mut==key_mutrg){
	MutrgHit *mutrg_hit=mutrg_itr->second;
	float q=mut_hit->get_q();
	float qerr=mut_hit->get_error_q();
	float ptime=mut_hit->get_t(); // need to modify
	mutrg_hit->SetMutHitValue(gap,q,qerr,ptime);
      }
      else if(key_mut>key_mutrg){continue;} // look at next mutrg_hit
      else if(key_mut<key_mutrg){break;}    // look at next mut_hit
    }
  }

  return 0;
}

/////////////////////////////////////////////////////////////////
