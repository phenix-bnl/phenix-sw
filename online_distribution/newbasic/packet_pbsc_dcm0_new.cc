#include <packet_pbsc_dcm0_new.h>

Packet_pbsc_dcm0_new::Packet_pbsc_dcm0_new(PACKET_ptr data)
  : Packet_pbsc_dcm0 (data){}
  
int* Packet_pbsc_dcm0_new::decode ( int *nwout)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>=144 || nchannels <=0){
    *nwout=0;
    return NULL;
  }
  *nwout=144*5;
  int *p=new int[*nwout];
  // we clear the output vector if NLEN is not 0
  memset(p,0,(*nwout)*sizeof(int)); 

  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      *nwout = 0;
      return 0;
    }

  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

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

      emc++;
    }
  return p;
}
// ------------------------------------------------------

int *Packet_pbsc_dcm0_new::decode_amu (int *nwout)
{
  int *p,*k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[3];
  for (int i = 0; i<3; i++)
    p[i] = k[5+i] & 0x3f;
  *nwout = 3;
  return p;
}

// ------------------------------------------------------

int *Packet_pbsc_dcm0_new::decode_misc (int *nwout)
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

  p[0] = k[2] & 0xffff;  // Event number
  p[1] = k[1];           // Module Address
  p[2] = k[3] & 0xffff;    // Beam Clock Counter


    for (i = 0; i<9; i++)
      p[3+i] = k[dlength - EMC_SUPPRESSED_DATA_TRAILER_LENGTH];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

int  Packet_pbsc_dcm0_new::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if (strcmp(what,"AMU") == 0)  // user requested AMU cells info
    {			
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_amu(&data2_length))==NULL)
	    return 0;
	}
      if (ich > data2_length) return 0;

      return decoded_data2[ich];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[0];
    }

  else if ( strcmp(what,"USERWORD")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      if (ich > 7 || ich < 0) return 0;

      return decoded_data3[3+ich];
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[1];
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[2];
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[11];
    }


  else	return 0;
}


// ------------------------------------------------------

int   Packet_pbsc_dcm0_new::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  if (ich >= 144) return 0;
  if (iy >= 5) return 0;
  return decoded_data1[ich*5 + iy];
}

// ------------------------------------------------------

void Packet_pbsc_dcm0_new::dump ( OSTREAM &os) 
{
  int *p,*k;
  int i, channel;

  this->identify(os);

  int dlength = getDataLength();

  COUT << "dlength = " << dlength << std::endl;
  if ( dlength < 450 ) return ;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }


  os << "  Iflag word :        "  << SETW(8) << std::hex << ( *k++ & 0xffff) << std::dec << std::endl;             //  ???
  os << "  Module  :           "  << SETW(8) << std::hex << ( *k++ & 0xffff) << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << ( *k++ & 0xffff) << std::dec << std::endl;    // Event number
  os << "  Clock:              "  << SETW(8) << std::hex << ( *k++ & 0xffff) << std::dec << std::endl;             // Module Address
  os << "  Det id:             "  << SETW(8) << std::hex << ( *k++ & 0xffff) << std::dec << std::endl;             //  ???
  os << "  AMU cell 1:         "  << SETW(8) << std::hex << ( *k++ & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 2:         "  << SETW(8) << std::hex << ( *k++ & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 3:         "  << SETW(8) << std::hex << ( *k++ & 0x3f) << std::dec   << std::endl;    // AMU cell 0

  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_SUPPRESSED_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  os <<"Number of hit channels="<<nchannels<<std::endl;
  for(i=0;i<nchannels;i++) {
    os<<std::dec<<"  ch="<<SETW(3)((*k>>20)&0xff);
    if((*k)&0x1000)
      os<<" Low ";
    else
      os<<" High";
    os<<" Post=0x"<<SETW(8)<<std::hex<<(*k++)&0xfff;
    os<<" Pre=0x" <<SETW(8)<<std::hex<<(*k++)&0xfff;
    os<<" Time=0x"<<SETW(8)<<std::hex<<(*k++)&0xfff<<std::endl;
  }
  for (i = 0; i<8; i++)
    os<<"  Userword"<<std::dec<<SETW(1)<<i<<" 0x"<<SETW(8)<<std::hex<<*k++<<std::endl;

  os<<"  Long. Parity word 0x"<<SETW(8)<<std::hex<<*k++<< std::endl;
  os<<"          Last word 0x"<<SETW(8)<<std::hex<<*k++<< std::endl;

  dumpErrorBlock(os);
  dumpDebugBlock(os);
}

int Packet_pbsc_dcm0_new::fillList5(int* rawData, int* address, int addressOffset)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>=144 || nchannels<=0) return 0;
  int* iniaddress=address;
  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

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
	}

      emc++;
    }
  return (int)(address-iniaddress)/sizeof(int);
}
void Packet_pbsc_dcm0_new::fillArray144x5(int* rawData)
{ /// Fills EMC array 144x5: 144x{time, high post, low post, high pre, low pre} 
  int i, channel, index;
  int dlength = getDataLength();
  memset(rawData,0,144*5*sizeof(int));
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>=144 || nchannels<=0) return;
  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }

  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);
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
}
int Packet_pbsc_dcm0_new::fillCoarseEnergyList(int* Energy, int* address, int addressOffset)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>=144 || nchannels<=0) return 0;
  int* iniaddress=address;
  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

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
void Packet_pbsc_dcm0_new::fillCoarseEnergyArray(int* Energy)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  memset(Energy,0,144*sizeof(int));
  if(nchannels>=144 || nchannels<=0) return;
  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

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
}
int Packet_pbsc_dcm0_new::fillQtileList(int* rawData, int* address, int addressOffset)
{
  int i, channel;
  int dlength = getDataLength();  
  dlength -= (EMC_SUPPRESSED_DATA_HEADER_LENGTH +
		 EMC_DATA_TRAILER_LENGTH);
  int nchannels=dlength/EMC_WORDS_PER_CH_SHORT;
  if(nchannels>=144) return 0;
  int* iniaddress=address;
  int* packetData=findPacketDataStart(packet);
  if (packetData == 0) 
    {
      return 0;
    }
  emchannel emc = (emchannel) (packetData+EMC_SUPPRESSED_DATA_HEADER_LENGTH);

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
	}

      emc++;
    }
  return (int)(address-iniaddress)/sizeof(int);
}
  





