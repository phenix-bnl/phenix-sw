#include "MutrgUnpack.hh"

#include "packet.h"
#include "MutrgHeader.hh"

const int MutrgUnpack::MAX_DATA_LENGTH=1024;

using namespace std;

////////////////////////////////////////////////////////////

MutrgUnpack::MutrgUnpack(void){
  class_name="MutrgUnpack";
  Reset();
}

//////////////////////////////////////////////////////////

void MutrgUnpack::Reset(void){
  header.clear();
  footer.clear();
  data.clear();
  packet_format=-1;
  return;
}

//////////////////////////////////////////////////////////

int MutrgUnpack::Unpack(Packet *packet){
  Reset();
  packet_format=packet->getHitFormat();

  if     (packet_format== 791){return UnpackF0(packet,5,1);}
  else if(packet_format==1291){return UnpackF1(packet,5,1);}
  else if(packet_format==1391){return UnpackF1(packet,5,1);}
  else{
    printf("Error - %s::Unpack : ",ClassName());
    printf("Unknown format number (%d)\n",packet_format);
    return -1;
  }
}

////////////////////////////////////////////////////////////

int MutrgUnpack::UnpackF0(Packet *packet,int nheader,int nfooter){
  const unsigned int BIT_BEGIN=0x40000000;
  const unsigned int BIT_END  =0x80000000;

  int array[MAX_DATA_LENGTH];
  int nget=CheckPacket(packet,array);
  if(nget<0){return -1;}

  // find data begin
  int data_num_beg=-1;
  for(int id=0; id<nget; id++){
    unsigned int uarray=(unsigned int)array[id];
    if(((uarray>>16)&0x4000)>0){data_num_beg=id; break;}
  }

  if(data_num_beg<0){
    printf("Error - %s::UnpackF0 : Couldn't find data begin flag.\n",
	   ClassName());
    return -1;
  }
  else if(data_num_beg!=0){
    printf("Warning - %s::UnpackF0 : data_num_beg=%d\n",
	   ClassName(),data_num_beg);
  }

  // find data end
  int data_num_end=-1;
  for(int id=nget-1; id>=0; id--){
    unsigned int uarray=(unsigned int)array[id];
    if(((uarray>>16)&0x8000)>0){data_num_end=id; break;}
  }

  if(data_num_end<0){
    printf("Error - %s::UnpackF0 : Couldn't find data end flag.\n",
	   ClassName());
    return -1;
  }
  else if(data_num_end!=nget-1){
    printf("Error - %s::UnpackF0 : data_num_end=%d, nget=%d\n",
	   ClassName(),data_num_end,nget);
    return -1;
  }

  // fill header
  for(int id=0; id<nheader; id++){
    header.push_back((unsigned int)array[data_num_beg+id]);
  }

  // fill footer
  for(int id=0; id<nfooter; id++){
    footer.push_back((unsigned int)array[nget-nfooter+id]);
  }

  // fill data
  int ndata=nget-data_num_beg-nheader-nfooter;
  for(int id=0; id<ndata; id++){
    unsigned int uarray=(unsigned int)array[data_num_beg+nheader+id];
    if(uarray&BIT_BEGIN || uarray&BIT_END){
      printf("Warning - %s::UnpackF0 : Unexpected data (%8.8x)\n",
	     ClassName(),uarray);
    }

    data.push_back(uarray);
  }

  // fill data number
  for(unsigned int id=0; id<data.size(); id++){
    data[id]&=0x0000ffff;
    data[id]|=(id<<16);
  }

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgUnpack::UnpackF1(Packet *packet,int nheader,int nfooter){
  const unsigned int BIT_BEGIN =0x40000000;
  const unsigned int BIT_END   =0x80000000;
  const unsigned int BIT_HEADER=0x20000000;

  int array[MAX_DATA_LENGTH];
  int nget=CheckPacket(packet,array);
  if(nget<0){return -1;}

  // find data begin
  int data_num_beg=-1;
  for(int id=0; id<nget; id++){
    unsigned int uarray=(unsigned int)array[id];
    if((uarray&BIT_BEGIN)>0){data_num_beg=id; break;}
  }

  if(data_num_beg<0){
    printf("Error - %s::UnpackF1 : Couldn't find data begin flag.\n",
	   ClassName());
    return -1;
  }
  else if(data_num_beg!=0){
    printf("Warning - %s::UnpackF1 : data_num_beg=%d\n",
	   ClassName(),data_num_beg);
  }

  // find data end
  int data_num_end=-1;
  for(int id=nget-1; id>=0; id--){
    unsigned int uarray=(unsigned int)array[id];
    if((uarray&BIT_END)>0){data_num_end=id; break;}
  }

  if(data_num_end<0){
    printf("Error - %s::UnpackF1 : Couldn't find data end flag.\n",
	   ClassName());
    return -1;
  }

  // fill header
  for(int id=data_num_beg; id<nget; id++){
    unsigned int uarray=(unsigned int)array[id];
    if((uarray&BIT_HEADER)==0){break;}
    header.push_back(uarray);
  }

  if(header.size()!=(unsigned int)nheader){
    printf("Error - %s::UnpackF1 : Unexpected header size (%d!=%d)\n",
	   ClassName(),nheader,(int)header.size());
    return -1;
  }

  // fill footer
  for(int id=0; id<nfooter; id++){
    footer.push_back((unsigned int)array[nget-nfooter+id]);
  }

  // fill data
  int ndata=nget-data_num_beg-nheader-nfooter;
  for(int id=0; id<ndata; id++){
    unsigned int uarray=array[data_num_beg+nheader+id];
    if(uarray&BIT_BEGIN || uarray&BIT_END || uarray&BIT_HEADER){
      printf("Warning - %s::UnpackF0 : Unexpected data (%8.8x)\n",
	     ClassName(),uarray);
    }

    data.push_back(uarray);
  }

  return 0;
}

///////////////////////////////////////////////////////////

int MutrgUnpack::CheckPacket(Packet *packet,int data[MAX_DATA_LENGTH]){
  int nget=packet->getDataLength();
  if(nget>MAX_DATA_LENGTH){
    printf("Error - %s::CheckPacket : ",ClassName());
    printf("Data length (%d) exceed MAX_DATA_LENGTH\n",nget);
    return -1;
  }

  int ret=packet->fillIntArray(data,MAX_DATA_LENGTH,&nget,"DATA");
  if(ret || nget!=packet->getDataLength()){
    printf("Error - %s::CheckPacket : ",ClassName());
    printf("Fail to get data array (ret=%d, nget=%d)\n",ret,nget);
    return -1;
  }

  return nget;
}

////////////////////////////////////////////////////////////

int MutrgUnpack::FillHeader(MutrgHeader *mutrg_header){
  if     (packet_format== 791){return FillHeaderF0(mutrg_header);}
  else if(packet_format==1291){return FillHeaderF1(mutrg_header);}
  else if(packet_format==1391){return FillHeaderF1(mutrg_header);}
  else{
    printf("Error - %s::FillHeader : ",ClassName());
    printf("Unknown format number (%d)\n",packet_format);
    return -1;
  }
}

///////////////////////////////////////////////////////////

int MutrgUnpack::FillHeaderF0(MutrgHeader *mutrg_header){
  if(CheckHeaderSize(5,1)){return -1;}
  mutrg_header->SetPacketFormat((unsigned short)packet_format);
  mutrg_header->SetEventNumber((unsigned short)(header[0]&0xffff));
  mutrg_header->SetFlagWord((unsigned short)(header[1]&0xffff));
  mutrg_header->SetDetectorID((unsigned short)(header[2]&0xffff));
  mutrg_header->SetModuleAddress((unsigned short)(header[3]&0xffff));
  mutrg_header->SetClockCounter((unsigned short)(header[4]&0xffff));
  mutrg_header->SetParityOK(true); // must be filled if needed
  mutrg_header->SetParityWord((unsigned short)(footer[0]&0xffff));
  mutrg_header->SetPacketNWord(0);
  return 0;
}

//////////////////////////////////////////////////////////

int MutrgUnpack::FillHeaderF1(MutrgHeader *mutrg_header){
  if(CheckHeaderSize(5,1)){return -1;}
  mutrg_header->SetPacketFormat((unsigned short)packet_format);
  mutrg_header->SetEventNumber((unsigned short)(header[0]&0xffff));
  mutrg_header->SetFlagWord((unsigned short)(header[1]&0xffff));
  mutrg_header->SetDetectorID((unsigned short)(header[2]&0xffff));
  mutrg_header->SetModuleAddress((unsigned short)(header[3]&0xffff));
  mutrg_header->SetClockCounter((unsigned short)(header[4]&0xffff));
  mutrg_header->SetParityOK((bool)(footer[0]&0x1));
  mutrg_header->SetPacketNWord((unsigned short)((footer[0]>>8)&0xf));
  mutrg_header->SetParityWord((unsigned short)((footer[0]>>12)&0xffff));
  return 0;
}

///////////////////////////////////////////////////////////

int MutrgUnpack::CheckHeaderSize(int nheader,int nfooter){
  if(header.size()<(unsigned int)nheader ||
     footer.size()<(unsigned int)nfooter){
    printf("Error - %s::FillHeaderF0 : ",ClassName());
    printf("Size of header(%d) or/and footer(%d) is inconsistent.\n",
	   (int)header.size(),(int)footer.size());
    return -1;
  }

  return 0;
}

///////////////////////////////////////////////////////////
