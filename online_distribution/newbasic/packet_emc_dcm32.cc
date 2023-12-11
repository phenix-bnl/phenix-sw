#include <packet_emc_dcm32.h>

Packet_emc_dcm32::Packet_emc_dcm32(PACKET_ptr data) 
: Packet_emc (data){max_channels=192;}

int *Packet_emc_dcm32::decode ( int *nwout)
{
  int i,j;
  int dlength = getDataLength();  
  // we clear the output vector if NLEN is not 0
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  *nwout=nchannels*5;
  int *p=new int[*nwout];
  int* iarr=p;
  //  memset(iarr,0,(*nwout)*sizeof(int)); //not necessary because we'll fill all fields
  PHDWORD* datap=findPacketDataStart(packet);
  if (datap == 0) 
    {
      *nwout = 0;
      return 0;
    }
  datap+=EMC_LONG_DATA_HEADER_LENGTH; 

  for (i=0; i < 5*192 ; i+=5*16)
    {
      for (j=0; j < 5*12 ; j++)
	{
	  *iarr = 4095 - (datap[ i + j ] &0xfff);
	  iarr++;
	}
    }
  return p;
}

// ------------------------------------------------------

int *Packet_emc_dcm32::decode_amu (int *nwout)
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
  for (int i = 0; i<3; i++)
    p[i] = k[6+i] & 0x3f;
  *nwout = 3;
  return p;
}

// ------------------------------------------------------

int *Packet_emc_dcm32::decode_misc (int *nwout)
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

void Packet_emc_dcm32::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  if ( dlength < 970 ) return ;

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


  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[969+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[977] << std::dec << std::endl;

  int j,l;

  for (j=0; j<144; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<5;l++) os  << SETW(8) << iValue(j,l) << " " ;
      os << std::dec << std::endl;
    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}

/* $$$$$$$$$$$$$ functions for long format data $$$$$$$$$$$$$ */

int Packet_emc_dcm32::filliList5x144(int** rawData, int* address,  int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put only real channels (144 out of 192) into the list with channel numbers 0-143 
  int i,j, index, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < 12 ; i++)
  {
    index=i*12;
    for(j=0;j<12;j++)
      {
	pre=(emc->highpre) & 0xfff;
	post=(emc->highpost) & 0xfff;
	if((pre - post) > threshold || post == 4095)
	  {
	    *address++=index+j+addressOffset;
	    rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	    rawData[1][nch+arrayOffset] = 4095-post;
	    rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	    rawData[3][nch+arrayOffset] = 4095-pre;
	    rawData[4][nch+arrayOffset] = 4095-((emc->lowpre) & 0xfff);
	    nch++;
	  }
	emc++;
      }
    emc+=4;
  }
  return nch;
}
int Packet_emc_dcm32::fillfList5x144(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put only real channels (144 out of 192) into the list with channel numbers 0-143 
  int i,j, index, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < 12 ; i++)
  {
    index=i*12;
    for(j=0;j<12;j++)
      {
	pre=(emc->highpre) & 0xfff;
	post=(emc->highpost) & 0xfff;
	if((pre - post) > threshold || post == 4095)
	  {
	    *address++=index+j+addressOffset;
	    rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	    rawData[1][nch+arrayOffset] = 4095-post;
	    rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	    rawData[3][nch+arrayOffset] = 4095-pre;
	    rawData[4][nch+arrayOffset] = 4095-((emc->lowpre) & 0xfff);
	    nch++;
	  }
	emc++;
      }
    emc+=4;
  }
  return nch;
}
int Packet_emc_dcm32::filliList6x144(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put only real channels (144 out of 192) into the list with channel numbers 0-143 
  int i,j, index, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < 12 ; i++)
  {
    index=i*12;
    for(j=0;j<12;j++)
      {
	pre=(emc->highpre) & 0xfff;
	post=(emc->highpost) & 0xfff;
	if((pre - post) > threshold || post == 4095)
	  {
	    rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	    rawData[1][nch+arrayOffset] = 4095-post;
	    rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	    rawData[3][nch+arrayOffset] = 4095-pre;
	    rawData[4][nch+arrayOffset] = 4095-((emc->lowpre) & 0xfff);
	    rawData[5][nch+arrayOffset] = index+j+addressOffset;
	    nch++;
	  }
	emc++;
      }
    emc+=4;
  }
  return nch;
}
int Packet_emc_dcm32::filliList144x6(int* rawData, int addressOffset=0, int threshold=-4096)
{// we put only real channels (144 out of 192) into the list with channel numbers 0-143 
  int i,j, index, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < 12 ; i++)
  {
    index=i*12;
    for(j=0;j<12;j++)
      {
	pre=(emc->highpre) & 0xfff;
	post=(emc->highpost) & 0xfff;
	if((pre - post) > threshold || post == 4095)
	  {
	    *rawData++ = 4095-((emc->time) & 0xfff);
	    *rawData++ = 4095-post;
	    *rawData++ = 4095-((emc->lowpost) & 0xfff);
	    *rawData++ = 4095-pre;
	    *rawData++ = 4095-((emc->lowpre) & 0xfff);
	    *rawData++ = index+j+addressOffset;
	    nch++;
	  }
	emc++;
      }
    emc+=4;
  }
  return nch;
}
int Packet_emc_dcm32::filliList5x192(int** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put all channels  into the list with channel numbers 0-191 
  int i, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels; i++)
  {
    pre=(emc->highpre) & 0xfff;
    post=(emc->highpost) & 0xfff;
    if( (pre - post) > threshold || post == 4095)
      {
	*address++=i+addressOffset;
	rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	rawData[1][nch+arrayOffset] = 4095-post;
	rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	rawData[3][nch+arrayOffset] = 4095-pre;
	rawData[4][nch+arrayOffset] = ((emc->lowpre) & 0xfff);
	nch++;
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcm32::fillfList5x192(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put all channels  into the list with channel numbers 0-191 
  int i, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels; i++)
  {
    pre=(emc->highpre) & 0xfff;
    post=(emc->highpost) & 0xfff;
    if((pre - post) > threshold || post == 4095)
      {
	*address++=i+addressOffset;
	rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	rawData[1][nch+arrayOffset] = 4095-post;
	rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	rawData[3][nch+arrayOffset] = 4095-pre;
	rawData[4][nch+arrayOffset] = 4095-((emc->lowpre) & 0xfff);
	nch++;
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcm32::filliList6x192(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{// we put all channels  into the list with channel numbers 0-191 
  int i, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels; i++)
  {
    pre=(emc->highpre) & 0xfff;
    post=(emc->highpost) & 0xfff;
    if((pre - post) > threshold || post == 4095)
      {
	rawData[0][nch+arrayOffset] = 4095-((emc->time) & 0xfff);
	rawData[1][nch+arrayOffset] = 4095-post;
	rawData[2][nch+arrayOffset] = 4095-((emc->lowpost) & 0xfff);
	rawData[3][nch+arrayOffset] = 4095-pre;
	rawData[4][nch+arrayOffset] = 4095-((emc->lowpre) & 0xfff);
	rawData[5][nch+arrayOffset] = i+addressOffset;
	nch++;
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcm32::filliList192x6(int* rawData, int addressOffset=0, int threshold=-4096)
{// we put all channels  into the list with channel numbers 0-191 
  int i, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels; i++)
  {
    pre=(emc->highpre) & 0xfff;
    post=(emc->highpost) & 0xfff;
    if((pre - post) > threshold || post ==4095)
      {
	*rawData++ = 4095-((emc->time) & 0xfff);
	*rawData++ = 4095-post;
	*rawData++ = 4095-((emc->lowpost) & 0xfff);
	*rawData++ = 4095-pre;
	*rawData++ = 4095-((emc->lowpre) & 0xfff);
	*rawData++ = i+addressOffset;
	nch++;
      }
    emc++;
  }
  return nch;
}
int Packet_emc_dcm32::fillArray192x5(int* rawData)
{ /// Fills EMC array 192x5: 144x{time, high post, low post, high pre, low pre} 
  int i;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  int* emc = (int*) (packetData+EMC_LONG_DATA_HEADER_LENGTH);
  for (i=0; i < nchannels ; i++)
  {
    *rawData++ =4095-( *(emc++) & 0xfff);
  }
  return nchannels;
}
int Packet_emc_dcm32::fillArray144x5(int* rawData)
{ /// Fills EMC array 144x5: 144x{time, high post, low post, high pre, low pre} 
  int i, j;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc = (emclong) (packetData+EMC_LONG_DATA_HEADER_LENGTH);
  for (i=0; i < 12; i++)
  {
    for(j=0;j<12;j++)
      {
	*rawData++ = 4095-((emc->time) & 0xfff);
	*rawData++ = 4095-((emc->highpost) & 0xfff);
	*rawData++ = 4095-((emc->lowpost) & 0xfff);
	*rawData++ = 4095-((emc->highpre) & 0xfff);
	*rawData++ = 4095-((emc->lowpre) & 0xfff);
	emc++;
      }
    emc+=4;
  }
  return 144;
}
int Packet_emc_dcm32::fillCoarseEnergyList(int* Energy, int* address, int addressOffset=0, int threshold=-4096)
{
  int i, j, en, pre, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc = (emclong) (packetData+EMC_LONG_DATA_HEADER_LENGTH);
  for (i=0; i < 12; i++)
  {
    int index=i*12;
    for(j=0;j<12;j++)
      {
	pre=(emc->highpre) & 0xfff;
	post=(emc->highpost) & 0xfff;
	if(post<MIN_HIGH_GAIN || post == 4095) // low gain
	  {
	    *address++=index+j+addressOffset;
	    *Energy++ = ((emc->lowpre & 0xfff)-(emc->lowpost & 0xfff))<<4;
	  }
	else // high gain
	  {
	    en=pre-post;
	    if(en > threshold) {
	      *address++=index+j+addressOffset;
	      *Energy++ = en;
	    }
	  }
	emc++;
      }
    emc+=4;
  }
  return (int)(address-iniaddress)/sizeof(int);
}
int Packet_emc_dcm32::fillCoarseEnergyArray(int* Energy)
{
  int i, j, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  for (i=0; i < 12; i++)
  {
    for(j=0;j<12;j++)
      {
	post=emc->highpost & 0xfff;
	if (post < MIN_HIGH_GAIN || post == 4095) // low gain
	    {
	      *Energy++ = ((emc->lowpre & 0xfff)-(emc->lowpost & 0xfff))<<4;
	    }

	  else // high gain
	    {
	      *Energy++ = (emc->highpre & 0xfff)-post;
	    }
	emc++;
      }
    emc+=4;
  }
  return 144;
}

int Packet_emc_dcm32::fillQtileArray(int* qtileEnergy)
{
  int i, j, k, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);

  for (i=0; i < 12; i++)
    {
      
      for(j=0;j<3;j++)
	{
	  *qtileEnergy=0;
	  for(k=0;k<4;k++)
	    {
	      post=emc->highpost & 0xfff;
	      if ( post < MIN_HIGH_GAIN || post == 4095) // low gain
		{
		  *qtileEnergy +=((emc->lowpre & 0xfff)-(emc->lowpost & 0xfff))<<4;
		}
	      else // high gain
		{
		  *qtileEnergy +=((emc->highpre & 0xfff) - post);
		}
	      emc++;
	    } // end for k
	  qtileEnergy++;
	} // end for j
      emc+=4; // skip disconnected AMUADC channels 
    }
  return 36;
}

int Packet_emc_dcm32::fillQtileList(int* qtileEnergy, int* address, int addressOffset=0, int threshold=0)
{
  int i,j,k, post;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_LONG_DATA_HEADER_LENGTH +
	      EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_LONG;
  if(nchannels!=192) {
    COUT<<"Hit Format "<<getHitFormat()<<"is not long format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emclong emc=(emclong)(packetData+EMC_LONG_DATA_HEADER_LENGTH);
  for (i=0; i < 12; i++)
  {
    int index=i*3;
    for (j=0; j < 3; j++)
    {
      *qtileEnergy=0;
      for (k=0; k < 4; k++)
	{
	  post=emc->highpost & 0xfff;
	  if ( post < MIN_HIGH_GAIN || post == 4095) // low gain
	    {
	      *qtileEnergy+=((emc->lowpre & 0xfff)-(emc->lowpost & 0xfff))<<4;
	    }
	  else // high gain
	    {
	      *qtileEnergy+=((emc->highpre & 0xfff) - post);
	    }
	  emc++;
	}
      if(*qtileEnergy > threshold) {
	qtileEnergy++;
	*address=index+j+addressOffset;
	address++;
      }
    }
    emc+=4;
  }
  return (int)(address-iniaddress)/sizeof(int);
}


int   Packet_emc_dcm32::fillIntArray (int iarr[],
				      const int nlen, int *nwout,
				      const char *what)
{
  int *from;
  int howmuch;

  *nwout = 0;


  // the fast trigger routine 
  if (strcmp(what,"SPARSE") == 0)
    {
      int uu =  decode_to_sparse(iarr);
      *nwout = 6*uu;
      return uu;
    }
    

  else if (strcmp(what,"") == 0)
    {
      // now let's derefence the proxy array. If we didn't decode
      // the data until now, we do it now
      if (decoded_data1 == NULL )
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            {
              *nwout=0;
              return -1;
            }
        }
      howmuch = data1_length;
      from = decoded_data1;
      

    }

  else if (strcmp(what,"RAW") == 0)
    {
      if (decoded_data1 == NULL )
        {
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    {
	      *nwout=0;
	      return -1;
	    }
	}
      howmuch = getLength();
      from = (int *) packet;
    }

  else if (strcmp(what,"DATA") == 0)
    {   
      
      if (decoded_data1 == NULL )
        {
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    {
	      *nwout=0;
	      return -1;
	    }
	}
      howmuch = getDataLength();
      from = (int *) findPacketDataStart(packet);
      
    }
  
  else
    {
      *nwout = 0;
      return 0;
    }

  // see if by any chance we got a negative length (happens)
  if (howmuch < 0)
    {
      *nwout = 0;
      return -3;
    }

  // see if our array is long enough
  if (nlen < howmuch) 
    {
      *nwout = 0;
      return -2;
    }

  if ( from == 0) 
    { 
      *nwout = 0;
      return -1;
    }


  // and copy the data to the output array
  //  for (int i=0; i<howmuch; i++) *iarr++ = *from++;
  memcpy(iarr, from, 4*howmuch);
      
  // tell how much we copied
  *nwout = howmuch;
  return 0;
      
  
}


int Packet_emc_dcm32::decode_to_sparse (int *p)
{


  int count = 0;
  int i,j;

  for (i=0; i< 144; i++)
    {
      p[i*6] = count;
      for (j=0; j< 5; j++)
	{
	  p[i*6+j+1] = iValue(i,j);
	}
      count++;
    }

  return count;
}



