#include "MutrgDecode.hh"

#include <vector>

#include "MutrgPar.hh"
#include "MutrgHeader.hh"
#include "MutrgHit.hh"
#include "MutrgHitArray.hh"

///////////////////////////////////////////////////////////

MutrgDecode::MutrgDecode(void){
  class_name="MutrgDecode";
  mutrg_header=NULL;
}

//////////////////////////////////////////////////////

void MutrgDecode::Reset(void){
  mutrg_header=NULL;
  data.clear();
}

///////////////////////////////////////////////////////

int MutrgDecode::Decode(int num,std::vector<unsigned int> &data_array,
			MutrgHeader *mh,MutrgHitArray *mutrg_hits){
  FillHeader(mh);
  SetMutrgHeader(mh);
  SetData(data_array);
  return Decode(mutrg_hits);
}

/////////////////////////////////////////////////////////

int MutrgDecode::Decode(MutrgHitArray *mutrg_hits){
  if(!mutrg_header){
    printf("Error - %s::Decode : mutrg_header=NULL\n", class_name.c_str());
    return -1;
  }

  unsigned short mrg_format=mutrg_header->GetMrgFormat();

  if(mrg_format==0x1){ // Run10
    return DecodeF0(mutrg_hits,0,1);
  }
  else if(mrg_format==0x0){ // Run9
    int ret;
    if((ret=DecodeF0(mutrg_hits,0,1))){return ret;}
    if((ret=MaskMutrgHit(mutrg_hits,0,2,4,1))){return ret;}
  }
  else if(mrg_format==0x8){
    return DecodeF0(mutrg_hits,0,1);
  }
  else{
    printf("Error - %s::Decode : ",class_name.c_str());
    printf("Unknown format (%d)\n",mrg_format);
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgDecode::DecodeF0(MutrgHitArray *mutrg_hits,int nheader,int nfooter){
  const int NOCT_PACKET=MutrgPar::NOCTANT_IN_PACKET; // # of octant in packet
  const int WORD=16;
  const int NWORD_SUM=38; // sum of NWORD_ST
  const int NWORD_ST[MutrgPar::NSTATION]={6,12,20}; // num of strip / 16
  const int MAX_EVT_WIDTH=7;
  const int NSTRIP_IN_WORD=16;

  if(!mutrg_header){
    printf("Error - %s::Decode : mutrg_header=NULL\n",class_name.c_str());
    return -1;
  }

  int arm=mutrg_header->GetArm();
  int octant[NOCT_PACKET]={
    mutrg_header->GetOctant(0),
    mutrg_header->GetOctant(1)
  };

  // rearrange data array
  unsigned int data_sort[NOCT_PACKET][NWORD_SUM][MAX_EVT_WIDTH];
  unsigned int data_flag[NOCT_PACKET][NWORD_SUM];
  for(int ioct=0; ioct<NOCT_PACKET; ioct++){
    for(int iword=0; iword<NWORD_SUM; iword++){
      for(int iwidth=0; iwidth<MAX_EVT_WIDTH; iwidth++){
	data_sort[ioct][iword][iwidth]=0;
      }
      data_flag[ioct][iword]=0;
    }
  }

  for(unsigned int id=0; id<data.size()-nfooter; id++){
    int data_num=(int)(data[id]>>WORD)-nheader;
    if(data_num<0){continue;} // skip header

    int evt_clk=data_num/(NOCT_PACKET*NWORD_SUM);
    int oct_num=(data_num/NWORD_SUM)%NOCT_PACKET;
    int data_num_oct=data_num%NWORD_SUM;
    if(evt_clk>=MAX_EVT_WIDTH){
      printf("Error - %s::DecodeF0 : ",class_name.c_str());
      printf("event clock (%d) exceed MAX_EVT_WIDTH (%d)\n",
	     evt_clk,MAX_EVT_WIDTH);
      return -1;
    }

    data_sort[oct_num][data_num_oct][evt_clk]=data[id];
    data_flag[oct_num][data_num_oct]|=(data[id]&0xffff);
  }

  // fill MuTRG hit
  for(int ioct=0; ioct<NOCT_PACKET; ioct++){
    if(octant[ioct]<0 || octant[ioct]>=MutrgPar::NOCTANT){continue;}

    for(int iword=0; iword<NWORD_SUM; iword++){
      if(!data_flag[ioct][iword]){continue;} // skip word without hit

      // calculate station number
      int station=-1;
      int data_num_st=-1;
      int ndata_pre=0;
      for(int ist=0; ist<MutrgPar::NSTATION; ist++){
	if(iword>=ndata_pre && iword<ndata_pre+NWORD_ST[ist]){
	  station=ist;
	  data_num_st=iword-ndata_pre;
	}
	ndata_pre+=NWORD_ST[ist];
      }

      for(int istp=0; istp<NSTRIP_IN_WORD; istp++){
	MutrgHit *mhit=NULL;

	// Calculate half octant and strip number
	int hoct,stp;
	if(station==2){
	  const int NWORD_HOCT=NWORD_ST[station]/2;
	  hoct=data_num_st/NWORD_HOCT;
	  stp=(data_num_st%NWORD_HOCT)*NSTRIP_IN_WORD+istp;
	}
	else{
	  int nstrip_hoct0=
	    MutrgPar::NSTRIP_IN_HALFOCTANT(arm,station,octant[ioct],0);
	  int stpo=data_num_st*NSTRIP_IN_WORD+istp;
	  hoct=(stpo/nstrip_hoct0 ? 1 : 0);
	  stp=stpo-hoct*nstrip_hoct0;
	}

	for(int iwidth=0; iwidth<MAX_EVT_WIDTH; iwidth++){
	  unsigned int data1=data_sort[ioct][iword][iwidth];
	  if(!((data1>>istp)&0x1)){continue;}

	  if(!mhit){
	    mhit=mutrg_hits->Insert(arm,station,octant[ioct],hoct,stp);}
	  mhit->SetHitClock(iwidth,true);
	}
      }
    }
  }

  return 0;
}

////////////////////////////////////////////////////////

int MutrgDecode::FillHeader(MutrgHeader *mh){
  unsigned short user_word=(unsigned short)data[data.size()-1];
  unsigned short mrg_format=user_word&0xf;

  mh->SetUserWord(user_word);
  mh->SetMrgFormat(mrg_format);

  // At first, look at user word and determine MRG format.
  // Then fill MutrgHeader following the format number
  if     (mrg_format==0x1){FillHeaderF2(mh);}
  else if(mrg_format==0x0){FillHeaderF1(mh);} // mask mirror data in South
  else if(mrg_format==0x8){FillHeaderF0(mh);}
  else{
    printf("Error - %s::Decode : ",class_name.c_str());
    printf("Unknown format (0x%x)\n",mrg_format);
    return -1;
  }
  
  return 0;
}

//////////////////////////////////////////////////////////

int MutrgDecode::FillHeaderF0(MutrgHeader *mh){
  // For preliminary test version
  if(data.size()<=0){
    printf("Error - %s::FillHeaderF0 : data size = 0\n",class_name.c_str());
    return -1;
  }

  mh->SetEventWidth(0); // so far no information in PRDFF

  //unsigned short user_word=mh->GetUserWord(); // so far unused
  unsigned short packet_id=mh->GetPacketID();
  unsigned short mod_addr=mh->GetModuleAddress();

  if(mod_addr==0x6444 && // run 269047
     packet_id==11500){mh->SetArmOctant(1,1,0);}
  else if(packet_id==11500){mh->SetArmOctant(1,0,1);}
  else if(packet_id==11501){mh->SetArmOctant(1,2,3);}
  else if(packet_id==11502){mh->SetArmOctant(1,4,5);}
  else if(packet_id==11503){mh->SetArmOctant(1,6,7);}
  else{
    printf("Error - %s::FillHeaderF0 : Unknown Packet ID.\n",
	   class_name.c_str());
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgDecode::FillHeaderF1(MutrgHeader *mh){
  // For Run9. South-Oct6 is mirror of Oct5. Mask it.
  if(data.size()<=0){
    printf("Error - %s::FillHeaderF1 : data size = 0\n",class_name.c_str());
    return -1;
  }

  unsigned short user_word=mh->GetUserWord();
  mh->SetEventWidth((user_word&0x0070)>>4);
  mh->SetDcmifError((user_word&0x0080)>>7);
  mh->SetMrgError((user_word&0xff00)>>8);

  unsigned short det_id=mh->GetDetectorID();
  unsigned short mod_addr=mh->GetModuleAddress();
  if     (det_id==0x1 && mod_addr==0x0){mh->SetArmOctant(1,0, 1);}
  else if(det_id==0x1 && mod_addr==0x1){mh->SetArmOctant(1,2, 3);}
  else if(det_id==0x1 && mod_addr==0x2){mh->SetArmOctant(1,4, 5);}
  else if(det_id==0x1 && mod_addr==0x3){mh->SetArmOctant(1,6, 7);}
  else if(det_id==0x0 && mod_addr==0x0){mh->SetArmOctant(0,4,-1);}
  else{
    printf("Error - %s::FillHeaderF1 : ",class_name.c_str());
    printf("Unknown Detector ID / Module Address.\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgDecode::FillHeaderF2(MutrgHeader *mh){
  // For full operation in future
  if(data.size()<=0){
    printf("Error - %s::FillHeaderF2 : data size = 0\n",class_name.c_str());
    return -1;
  }

  unsigned short user_word=mh->GetUserWord();
  mh->SetEventWidth((user_word&0x0070)>>4);
  mh->SetDcmifError((user_word&0x0080)>>7);
  mh->SetMrgError((user_word&0xff00)>>8);

  unsigned short det_id=mh->GetDetectorID();
  unsigned short mod_addr=mh->GetModuleAddress();
  if     (det_id==0x1 && mod_addr==0x0){mh->SetArmOctant(1,0,1);}
  else if(det_id==0x1 && mod_addr==0x1){mh->SetArmOctant(1,2,3);}
  else if(det_id==0x1 && mod_addr==0x2){mh->SetArmOctant(1,4,5);}
  else if(det_id==0x1 && mod_addr==0x3){mh->SetArmOctant(1,6,7);}
  else if(det_id==0x0 && mod_addr==0x0){mh->SetArmOctant(0,7,0);}
  else if(det_id==0x0 && mod_addr==0x1){mh->SetArmOctant(0,1,2);}
  else if(det_id==0x0 && mod_addr==0x2){mh->SetArmOctant(0,3,4);}
  else if(det_id==0x0 && mod_addr==0x3){mh->SetArmOctant(0,5,6);}
  else{
    printf("Error - %s::FillHeaderF2 : ",class_name.c_str());
    printf("Unknown Detector ID / Module Address.\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgDecode::FillHeaderF3(MutrgHeader *mh){
  // For debug mode
  if(data.size()<=0){
    printf("Error - %s::FillHeaderF3 : data size = 0\n",class_name.c_str());
    return -1;
  }

  unsigned short user_word=mh->GetUserWord();
  mh->SetEventWidth((user_word&0x0070)>>4);
  mh->SetDcmifError((user_word&0x0080)>>7);
  mh->SetMrgError((user_word&0xff00)>>8);

  unsigned short packet_id=mh->GetPacketID();
  if     (packet_id==11500){mh->SetArmOctant(1,0,1);}
  else if(packet_id==11501){mh->SetArmOctant(1,2,3);}
  else if(packet_id==11502){mh->SetArmOctant(1,4,5);}
  else if(packet_id==11503){mh->SetArmOctant(1,6,7);}
  else if(packet_id==11510){mh->SetArmOctant(0,7,0);}
  else if(packet_id==11511){mh->SetArmOctant(0,1,2);}
  else if(packet_id==11512){mh->SetArmOctant(0,3,4);}
  else if(packet_id==11513){mh->SetArmOctant(0,5,6);}
  else{
    printf("Error - %s::FillHeaderF3 : ",class_name.c_str());
    printf("Unknown Detector ID / Module Address.\n");
    return -1;
  }

  return 0;
}

/////////////////////////////////////////////////////////

int MutrgDecode::MaskMutrgHit(MutrgHitArray *mutrg_hits,
			      int arm,int station,int octant,int halfoctant){
  MutrgHitArray::const_private_itr_pair itr_pair=
    mutrg_hits->Range(arm,station,octant,halfoctant);
  MutrgHitArray::const_private_itr itr=itr_pair.first;
  MutrgHitArray::const_private_itr itr_end=itr_pair.second;

  std::vector<unsigned int> key_list;
  for(; itr!=itr_end; itr++){key_list.push_back(itr->first);}

  for(unsigned int ikey=0; ikey<key_list.size(); ikey++){
    mutrg_hits->Remove(key_list[ikey]);
  }

  return 0;
}

///////////////////////////////////////////////////////////
