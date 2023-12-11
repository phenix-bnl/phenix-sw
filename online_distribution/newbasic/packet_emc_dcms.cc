#include <packet_emc_dcms.h>

Packet_emc_dcms::Packet_emc_dcms(PACKET_ptr data) 
: Packet_emc (data){max_channels=144;}

int *Packet_emc_dcms::decode ( int *nwout)
{
  int i;
  int dlength = getDataLength();  
  // we clear the output vector if NLEN is not 0
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  *nwout=nchannels*5;
  int *p=new int[*nwout];
  int* iarr=p;
  //  memset(iarr,0,(*nwout)*sizeof(int)); //not necessary because we'll fill all fields
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      *nwout = 0;
      return 0;
    }
  emcshort emc = (emcshort) (packetData+EMC_SHORT_DATA_HEADER_LENGTH); 

  for (i=0; i < nchannels ; i++)
    {
			
      if ( emc->post & 0x1000) // low gain
	{
	  *iarr++ = (emc->timing & 0xfff);
	  *iarr++ = 0;
	  *iarr++ = (emc->post & 0xfff);
	  *iarr++ = 0;
	  *iarr++ = (emc->pre & 0xfff);
	}

      else // high gain
	{
	  *iarr++ = (emc->timing & 0xfff);
	  *iarr++ = (emc->post & 0xfff);
	  *iarr++ = 0;
	  *iarr++ = (emc->pre & 0xfff);
	  *iarr++ = 0;
	}
    
      emc++;
    }
  return p;
}

// ------------------------------------------------------

int *Packet_emc_dcms::decode_amu (int *nwout)
{
  int *p,*k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  if(k[0]!=0xFFFF){
    *nwout=0;
    return 0;
  }
  p = new int[3];
  int amustart=EMC_SHORT_DATA_HEADER_LENGTH-3;
  for (int i = 0; i<3; i++)
    p[i] = k[amustart+i] & 0x3f;
  *nwout = 3;
  return p;
}

// ------------------------------------------------------

int *Packet_emc_dcms::decode_misc (int *nwout)
{
  int *p,*k;
  int i;

  int dlength = getDataLength();
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  if(k[0]!=0xFFFF){
    *nwout=0;
    return 0;
  }
  p = new int[12];

  p[0] = k[2] & 0xfffff;  // Event number
  p[1] = k[3];           // Module Address
  p[2] = k[5] & 0xff;    // Beam Clock Counter


  int userwordstart=dlength-EMC_DATA_TRAILER_LENGTH;
  for (i = 0; i<9; i++)
    p[3+i] = k[userwordstart+i];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

void Packet_emc_dcms::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=(dlength-
		 EMC_SHORT_DATA_HEADER_LENGTH-
		 EMC_DATA_TRAILER_LENGTH)/EMC_WORDS_PER_CH_SHORT;
  if(nchannels<0) return;
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }

  os << "  Start marker word : "  << SETW(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[1] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[2]  & 0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[3]  & 0xfffff) << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[4]  & 0xfffff) << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[5] & 0xfffff) << std::dec << std::endl;    // Beam Clock Counter
  os << "  AMU cell 1:         "  << SETW(8) << std::hex << (k[6] & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 2:         "  << SETW(8) << std::hex << (k[7] & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 3:         "  << SETW(8) << std::hex << (k[8] & 0x3f) << std::dec   << std::endl;    // AMU cell 0

  int userwortstart=dlength-EMC_DATA_TRAILER_LENGTH;
  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[userwortstart+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[userwortstart+i] << std::dec << std::endl;

  int j,l;

  for (j=0; j<nchannels; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<3;l++) 
	{
	  os << std::hex;	  
          os << SETW(3) << (k[EMC_SHORT_DATA_HEADER_LENGTH+3*j+l] & 0xfff)  << " " ;
	  if ( k[9+3*j+l] & 0x1000 ) os << "L    ";
	  else os << "H    ";
	}
      os << std::dec << std::endl;
    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}

/* $$$$$$$$$$$$$ functions for short format data $$$$$$$$$$$$$ */

int Packet_emc_dcms::filliList5x144(int** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	*address++=i+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  *address++=i+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::fillfList5x144(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	*address++=i+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  *address++=i+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::filliList6x144(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	rawData[5][nch+arrayOffset] = i+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  rawData[5][nch+arrayOffset] = i+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::filliList144x6(int* rawData, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	*rawData++ = (emc->timing & 0xfff);
	*rawData++ = 0; //high gain post
	*rawData++ = (emc->post & 0xfff); //low gain post
	*rawData++ = 0; //high gain pre
	*rawData++ = (emc->pre & 0xfff);
	*rawData++=i+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  *rawData++ = (emc->timing & 0xfff);
	  *rawData++ = post;
	  *rawData++ = 0;
	  *rawData++ = pre;
	  *rawData++ = 0;
	  *rawData++ = i+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::fillArray144x5(int* rawData)
{ /// Fills EMC array 144x5: 144x{time, high post, low post, high pre, low pre} 
  int i;
//  int dlength = getPacketDataLength(packet);  
  int dlength = getDataLength();  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc = (emcshort) (packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	*rawData++ = (emc->timing & 0xfff);
	*rawData++ = 0; //high gain post
	*rawData++ = (emc->post & 0xfff); //low gain post
	*rawData++ = 0; //high gain pre
	*rawData++ = (emc->pre & 0xfff);
      }
    
    else // high gain
      {
	*rawData++ = (emc->timing & 0xfff);
	*rawData++ = (emc->post & 0xfff);
	*rawData++ = 0;
	*rawData++ = (emc->pre & 0xfff);
	*rawData++ = 0;
      }
    
    emc++;
  }
  return nchannels;
}
//$$$$$$$$$$$$$$ 192 channels addressing $$$$$$$$$$$$$$$
int Packet_emc_dcms::filliList5x192(int** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	nch++;
	*address++=i+i/12*4+addressOffset;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  nch++;
	  *address++=i+i/12*4+addressOffset;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::fillfList5x192(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	nch++;
	*address++=i+i/12*4+addressOffset;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  nch++;
	  *address++=i+i/12*4+addressOffset;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::filliList6x192(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	rawData[1][nch+arrayOffset] = 0; //high gain post
	rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	rawData[3][nch+arrayOffset] = 0; //high gain pre
	rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	rawData[5][nch+arrayOffset] = i+i/12*4+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	  rawData[1][nch+arrayOffset] = post;
	  rawData[2][nch+arrayOffset] = 0;
	  rawData[3][nch+arrayOffset] = pre;
	  rawData[4][nch+arrayOffset] = 0;
	  rawData[4][nch+arrayOffset] = i+i/12*4+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcms::filliList192x6(int* rawData, int addressOffset=0, int threshold=-4096)
{
  int i,  pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	*rawData++ = (emc->timing & 0xfff);
	*rawData++ = 0; //high gain post
	*rawData++ = (emc->post & 0xfff); //low gain post
	*rawData++ = 0; //high gain pre
	*rawData++ = (emc->pre & 0xfff);
	*rawData++ = i+i/12*4+addressOffset;
	nch++;
      }
    
    else // high gain
      {
	pre=emc->pre & 0xfff;
	post=emc->post & 0xfff;
	if((pre-post)>threshold){
	  *rawData++ = (emc->timing & 0xfff);
	  *rawData++ = post;
	  *rawData++ = 0;
	  *rawData++ = pre;
	  *rawData++ = 0;
	  *rawData++ = i+i/12*4+addressOffset;
	  nch++;
	}
      }
    emc++;
  }
  return nch;
}

int Packet_emc_dcms::fillArray192x5(int* rawData)
{ /// Fills EMC array 192x5: 192x{time, high post, low post, high pre, low pre} 
  int i;
//  int dlength = getPacketDataLength(packet);  
  int dlength = getDataLength();  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  memset(rawData,0,192*5*sizeof(int));
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc = (emcshort) (packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  for (i=0; i < nchannels ; i++)
  {
    int channel=i+i/12*4;
    if ( emc->post & 0x1000) // low gain
      {
	rawData[channel] = (emc->timing & 0xfff);
	//	rawData[channel+1] = 0; //high gain post
	rawData[channel+2] = (emc->post & 0xfff); //low gain post
	//	rawData[channel+3] = 0; //high gain pre
	rawData[channel+4] = (emc->pre & 0xfff);
      }
    
    else // high gain
      {
	rawData[channel] = (emc->timing & 0xfff);
	rawData[channel+1] = (emc->post & 0xfff);
	//	rawData[channel+2] = 0;
	rawData[channel+3] = (emc->pre & 0xfff);
	//	rawData[channel+4] = 0;
      }
    
    emc++;
  }
  return nchannels;
}
int Packet_emc_dcms::fillCoarseEnergyList(int* Energy, int* address, int addressOffset=0, int threshold=-4096)
{
  int i, e;
  int dlength = getDataLength();  
  //int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc = (emcshort) (packetData+EMC_SHORT_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
  {
    if ( emc->post & 0x1000) // low gain
      {
	*address++=i+addressOffset;
	*Energy++ = ((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
      }
    
    else // high gain
      {
	e = (emc->pre & 0xfff)-(emc->post & 0xfff);
	if(e>threshold) {
	  *Energy++=e;
	  *address++=i+addressOffset;
	}
      }
    
    emc++;
  }
  return (int)(address-iniaddress)/sizeof(int);
}

int Packet_emc_dcms::fillCoarseEnergyArray(int* Energy)
{
  int i;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  //  memset(Energy,0,144*sizeof(int));
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
    {
      if ( emc->post & 0x1000) // low gain
	{
	  Energy[i] = ((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	}
      
      else // high gain
	{
	  Energy[i] = (emc->pre & 0xfff)-(emc->post & 0xfff);
	}
      
      emc++;
    }
  return nchannels;
}
int Packet_emc_dcms::fillQtileArray(int* qtileEnergy)
{
  int i, j;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);

  for (i=0; i < 36 ; i++)
    {
      *qtileEnergy=0;
      for(j=0;j<4;j++)
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	    }
	  else // high gain
	    {
	      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff));
	    }
	  emc++;
	}
      qtileEnergy++;
    }
  return 36;
}

int Packet_emc_dcms::fillQtileList(int* qtileEnergy, int* address, int addressOffset=0, int threshold=0)
{
  int i, j;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SHORT_DATA_HEADER_LENGTH +
	      EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels!=144) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not short format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emcshort emc=(emcshort)(packetData+EMC_SHORT_DATA_HEADER_LENGTH);
  for (i=0; i < 36; i++)
  {
    *qtileEnergy=0;
    for(j=0;j<4;j++)
      {
	if ( emc->post & 0x1000) // low gain
	  {
	    *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	  }
      
	else // high gain
	  {
	    *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff));
	  }      
	emc++;
      }
    if(*qtileEnergy > threshold)
      { 
	*address=i+addressOffset;
	qtileEnergy++;
	address++;
      }
  }
  return (int)(address-iniaddress)/sizeof(int);
}














