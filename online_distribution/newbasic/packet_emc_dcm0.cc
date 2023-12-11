#include <packet_emc_dcm0.h>

Packet_emc_dcm0::Packet_emc_dcm0(PACKET_ptr data) 
: Packet_emc (data){max_channels=144;}

int *Packet_emc_dcm0::decode ( int *nwout)
{
  int i, channel, index;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels <0){
    *nwout=0;
    return NULL;
  }
  *nwout=144*5;
  int *p=new int[*nwout];
  // we clear the output vector if NLEN is not 0
  memset(p,0,(*nwout)*sizeof(int)); 

  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      *nwout = 0;
      return 0;
    }
  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      if ( emc->timing & 0x90000  && emc->post &0xc0000 && emc->pre & 0xa0000 
	   &&  channel >= 0 && channel < 144 )
	{
	  index=channel*5;
	  if ( emc->post & 0x1000) // low gain
	    {
	      p[index] = (emc->timing & 0xfff);
	      //	      p[index+1] = 0; //not necessary: we already zeroed all fields with memset(...)
	      p[index+2] = (emc->post & 0xfff);
	      //	      p[index+3] = 0;
	      p[index+4] = (emc->pre & 0xfff);
	    }

	  else // high gain
	    {
	      p[index] = (emc->timing & 0xfff);
	      p[index+1] = (emc->post & 0xfff);
	      //	  p[index+2] = 0;
	      p[index+3] = (emc->pre & 0xfff);
	      //	  p[index+4] = 0;
	    }
	}
      emc++;
    }
  return p;
}
// ------------------------------------------------------

int *Packet_emc_dcm0::decode_amu (int *nwout)
{
  int *p,*k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[3];
  int amustart=EMC_SUPPRESSED_DATA_HEADER_LENGTH-3;
  for (int i = 0; i<3; i++)
    p[i] = k[amustart+i] & 0x3f;
  *nwout = 3;
  return p;
}
// ------------------------------------------------------

int *Packet_emc_dcm0::decode_misc (int *nwout)
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

  p = new int[12];

  p[0] = k[2] & 0xfffff;  // Event number
  p[1] = k[1];           // Module Address
  p[2] = k[3] & 0xff;    // Beam Clock Counter


  int userwordstart=dlength-EMC_DATA_TRAILER_LENGTH;
  for (i = 0; i<9; i++)
    p[3+i] = k[userwordstart+i];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

void Packet_emc_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  int nchannels=(dlength-
		 EMC_SUPPRESSED_DATA_HEADER_LENGTH-
		 EMC_DATA_TRAILER_LENGTH)/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return;
  }
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }


  os << "  Iflag word :        "  << SETW(8) << std::hex << ( k[0] & 0xffff) << std::dec <<std::endl;             //  ???
  os << "  Module  :           "  << SETW(8) << std::hex << ( k[1] & 0xffff) << std::dec <<std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << ( k[2] & 0xffff) << std::dec <<std::endl;    // Event number
  os << "  Clock:              "  << SETW(8) << std::hex << ( k[3] & 0xffff) << std::dec <<std::endl;             // Module Address
  os << "  Det id:             "  << SETW(8) << std::hex << ( k[4] & 0xffff) << std::dec <<std::endl;             //  ???
  os << "  AMU cell 1:         "  << SETW(8) << std::hex << ( k[5] & 0x3f) << std::dec   <<std::endl;    // AMU cell 0
  os << "  AMU cell 2:         "  << SETW(8) << std::hex << ( k[6] & 0x3f) << std::dec   <<std::endl;    // AMU cell 0
  os << "  AMU cell 3:         "  << SETW(8) << std::hex << ( k[7] & 0x3f) << std::dec   <<std::endl;    // AMU cell 0

  int userwordstart=dlength-EMC_DATA_TRAILER_LENGTH;
  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[userwordstart+i] << std::dec <<std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[userwordstart+i] << std::dec <<std::endl;

  emcshort emc = (emcshort) &k[8];

  for (i=0; i<nchannels; i++)
    {

      int channel= ( emc->timing >> 20) & 0xff;
      os << std::dec << SETW(5) << channel<< " |  ";	  
      os << std::hex << SETW(8) << ( emc->timing & 0xfff) << " " ;
      os << std::hex << SETW(8) << ( emc->post & 0xfff) << " " ;
      os << std::hex << SETW(8) << ( emc->pre & 0xfff) << " " ;
      if (  emc->post  & 0x1000 ) os << "L    ";
      else os << "H    ";
      os << std::dec <<std::endl;  
      emc++;
    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}

int Packet_emc_dcm0::filliList5x144(int** rawData, int* address, int arrayOffset=0,  int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      *address++=channel+addressOffset;
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *address++=channel+addressOffset;
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::fillfList5x144(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      *address++=channel+addressOffset;
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *address++=channel+addressOffset;
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::filliList6x144(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      rawData[5][nch+arrayOffset] = channel+addressOffset;
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  rawData[5][nch+arrayOffset] = channel+addressOffset;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::filliList144x6(int* rawData, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=rawData;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      *rawData++ = (emc->timing & 0xfff);
	      *rawData++ = 0; //high gain post
	      *rawData++ = (emc->post & 0xfff); //low gain post
	      *rawData++ = 0; //high gain pre
	      *rawData++ = (emc->pre & 0xfff);
	      *rawData++ = channel+addressOffset;
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *rawData++ = (emc->timing & 0xfff);
		  *rawData++ = post;
		  *rawData++ = 0;
		  *rawData++ = pre;
		  *rawData++ = 0;
		  *rawData++ = channel+addressOffset;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::fillArray144x5(int* rawData)
{ /// Fills EMC array 144x5: 144x{time, high post, low post, high pre, low pre} 
  int i, channel, index;
  int dlength = getDataLength();
  memset(rawData,0,144*5*sizeof(int));
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  index=channel*5;
	  if ( emc->post & 0x1000) // low gain
	    {
	      rawData[index] = (emc->timing & 0xfff);
	      // rawData[index+1] = 0; //high gain post
	      rawData[index+2] = (emc->post & 0xfff); //low gain post
	      //rawData[index+3] = 0; //high gain pre
	      rawData[index+4] = (emc->pre & 0xfff);
	    }

	  else // high gain
	    {
	      rawData[index] = (emc->timing & 0xfff);
	      rawData[index+1] = (emc->post & 0xfff);
	      //rawData[index+2] = 0;
	      rawData[index+3] = (emc->pre & 0xfff);
	      //rawData[index+4] = 0;
	    }
	}

      emc++;
    }
  return nchannels;
}

//$$$$$$$$$$$$$$ 192 channels addressing $$$$$$$$$$$$$$$$$$$$
int Packet_emc_dcm0::filliList5x192(int** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  channel+=channel/12*4;
	  if ( emc->post & 0x1000) // low gain
	    {
	      *address++=channel+addressOffset;
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *address++=channel+addressOffset;
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::fillfList5x192(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  channel+=channel/12*4;
	  if ( emc->post & 0x1000) // low gain
	    {
	      *address++=channel+addressOffset;
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *address++=channel+addressOffset;
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::filliList6x192(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  channel+=channel/12*4;
	  if ( emc->post & 0x1000) // low gain
	    {
	      rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
	      rawData[1][nch+arrayOffset] = 0; //high gain post
	      rawData[2][nch+arrayOffset] = (emc->post & 0xfff); //low gain post
	      rawData[3][nch+arrayOffset] = 0; //high gain pre
	      rawData[4][nch+arrayOffset] = (emc->pre & 0xfff);
	      rawData[5][nch+arrayOffset]=channel+addressOffset;
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  rawData[0][nch+arrayOffset] = (emc->timing & 0xfff);
		  rawData[1][nch+arrayOffset] = post;
		  rawData[2][nch+arrayOffset] = 0;
		  rawData[3][nch+arrayOffset] = pre;
		  rawData[4][nch+arrayOffset] = 0;
		  rawData[5][nch+arrayOffset]=channel+addressOffset;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::filliList192x6(int* rawData, int addressOffset=0, int threshold=-4096)
{
  int i, channel, pre, post;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  //  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  int nch=0;
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  channel+=channel/12*4;
	  if ( emc->post & 0x1000) // low gain
	    {
	      *rawData++ = (emc->timing & 0xfff);
	      *rawData++ = 0; //high gain post
	      *rawData++ = (emc->post & 0xfff); //low gain post
	      *rawData++ = 0; //high gain pre
	      *rawData++ = (emc->pre & 0xfff);
	      *rawData++=channel+addressOffset;
	      nch++;
	    }

	  else // high gain
	    {
	      post=(emc->post & 0xfff);
	      pre=(emc->pre & 0xfff);
	      if((pre-post)>threshold)
		{
		  *rawData++ = (emc->timing & 0xfff);
		  *rawData++ = post;
		  *rawData++ = 0;
		  *rawData++ = pre;
		  *rawData++ = 0;
		  *rawData++ = channel+addressOffset;
		  nch++;
		}
	    }
	}

      emc++;
    }
  return nch;
}
int Packet_emc_dcm0::fillArray192x5(int *rawData)
{
  int i, channel, index;
  int dlength = getDataLength();
  memset(rawData,0,192*5*sizeof(int));
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  channel+=channel/12*4;
	  index=channel*5;
	  if ( emc->post & 0x1000) // low gain
	    {
	      rawData[index] = (emc->timing & 0xfff);
	      // rawData[index+1] = 0; //high gain post
	      rawData[index+2] = (emc->post & 0xfff); //low gain post
	      //rawData[index+3] = 0; //high gain pre
	      rawData[index+4] = (emc->pre & 0xfff);
	    }

	  else // high gain
	    {
	      rawData[index] = (emc->timing & 0xfff);
	      rawData[index+1] = (emc->post & 0xfff);
	      //rawData[index+2] = 0;
	      rawData[index+3] = (emc->pre & 0xfff);
	      //rawData[index+4] = 0;
	    }
	}

      emc++;
    }
  return nchannels;
}
int Packet_emc_dcm0::fillCoarseEnergyList(int* Energy, int* address, int addressOffset=0, int threshold=-4096)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  *address++=channel+addressOffset;
	  if ( emc->post & 0x1000) // low gain
	    {
	      *Energy++ = ((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	    }

	  else // high gain
	    {
	      *Energy++ = (emc->pre & 0xfff)-(emc->post & 0xfff);
	    }
	}

      emc++;
    }
  return (int)(address-iniaddress)/sizeof(int);
}
int Packet_emc_dcm0::fillCoarseEnergyArray(int* Energy)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  memset(Energy,0,144*sizeof(int));
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc = (emcshort) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      Energy[channel] = ((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	    }

	  else // high gain
	    {
	      Energy[channel] = (emc->pre & 0xfff)-(emc->post & 0xfff);
	    }
	}

      emc++;
    }
  return nchannels;
}
int Packet_emc_dcm0::fillQtileArray(int* qtileEnergy)
{
  int i, channel;
  int dlength = getDataLength();  
  //  int dlength = getDataSize(packet);  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  memset(qtileEnergy,0,36*sizeof(int));
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc=(emcshort)(packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

  for (i=0; i < nchannels ; i++)
    {
      channel=( emc->timing >> 20) & 0xff;
      //      if ( (emc->timing & 0x90000)==0x90000  && (emc->post &0xc0000)==0xc0000
      //	   && (emc->pre & 0xa0000)==0xa0000) && channel >= 0 && channel < 144 )
      if(channel >= 0 && channel < 144 )
	{
	  if ( emc->post & 0x1000) // low gain
	    {
	      qtileEnergy[channel>>2]+=((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	    }
	  else // high gain
	    {
	      qtileEnergy[channel>>2]+=((emc->pre & 0xfff)-(emc->post & 0xfff));
	    }
	}	

      emc++;
    }
  return 36;
}

int Packet_emc_dcm0::fillQtileList(int* qtileEnergy, int* address, int addressOffset=0, int threshold=0)
{
	int i, prevtile, tile;
//  int dlength = getPacketDataLength(packet);  
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
	      EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>max_channels || nchannels<0) {
    COUT<<"Hit Format "<<getHitFormat()
	<<"is not zero suppressed format: nchannels="<<nchannels<<std::endl;
    return 0;
  }
  int* iniaddress=address;
  PHDWORD* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emcshort emc=(emcshort)(packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
  *qtileEnergy=0;
  prevtile=( emc->timing >> 22) & 0x3f;
  if( prevtile > 35 ) return 0;
  if ( emc->post & 0x1000) // low gain
    {
      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
    }
  else // high gain
    {
      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff));
    }
  *address = prevtile + addressOffset;
  emc++;
  for (i=1; i < nchannels ; i++)
    {
      tile=( emc->timing >> 22) & 0x3f;
      if(tile < 36 )
	{
	  if(tile!=prevtile)
	    {
	      prevtile=tile;
	      if(*qtileEnergy > threshold) {
		qtileEnergy++;
		address++;
	      }
	      *qtileEnergy=0;
	      *address=prevtile + addressOffset;
	    }
	  if ( emc->post & 0x1000) // low gain
	    {
	      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff))<<4;
	    }
	  else // high gain
	    {
	      *qtileEnergy +=((emc->pre & 0xfff)-(emc->post & 0xfff));
	    }
	}
      
      emc++;
    }
  if(*qtileEnergy > threshold) address++;
  return (int)(address-iniaddress)/sizeof(int);
}












