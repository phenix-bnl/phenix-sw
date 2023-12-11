#include <packet_emc_olddcm32.h>


Packet_emc_olddcm32::Packet_emc_olddcm32(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_emc_olddcm32::decode ( int *nwout)
{
  int i;
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

  for (i=0; i < *nwout ; i++)
    {
	  *iarr++ = (*datap++)&0xfff;
    }
  return p;
}

// ------------------------------------------------------

int *Packet_emc_olddcm32::decode_amu (int *nwout)
{
  int *p,*k;

  int dlength = getDataLength();
  if ( dlength < 970 ) 
    {
      *nwout = 0;
      return 0;
    }
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[3];
  for (int i = 0; i<3; i++)
    p[i] = k[6+i] & 0x3f;
  *nwout = 3;
  return p;
}

// ------------------------------------------------------

int *Packet_emc_olddcm32::decode_misc (int *nwout)
{
  int *p,*k;
  int i;

  int dlength = getDataLength();
  if ( dlength < 970 ) 
    {
      *nwout = 0;
      return 0;
    }
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[12];

  p[0] = k[2] & 0xfffff;  // Event number
  p[1] = k[3];           // Module Address
  p[2] = k[5] & 0xff;    // Beam Clock Counter


  for (i = 0; i<9; i++)
    p[3+i] = k[969+i];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

int  Packet_emc_olddcm32::iValue(const int ich, const char *what)
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

int   Packet_emc_olddcm32::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  if (ich >= 192) return 0;
  if (iy >= 5) return 0;
  return decoded_data1[ich*5 + iy];
}

// ------------------------------------------------------

void Packet_emc_olddcm32::dump ( std::ostream &os) 
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


  os << "  Start marker word : "  << std::setw(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Detector id:        "  << std::setw(8) << std::hex <<  k[1] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << std::setw(8) << std::hex << (k[2]  & 0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << std::setw(8) << std::hex << (k[3]  & 0xfffff) << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << std::setw(8) << std::hex << (k[4]  & 0xfffff) << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << std::setw(8) << std::hex << (k[5] & 0xfffff) << std::dec << std::endl;    // Beam Clock Counter
  os << "  AMU cell 1:         "  << std::setw(8) << std::hex << (k[6] & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 2:         "  << std::setw(8) << std::hex << (k[7] & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 3:         "  << std::setw(8) << std::hex << (k[8] & 0x3f) << std::dec   << std::endl;    // AMU cell 0


  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   std::setw(8) << std::hex <<  k[969+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   std::setw(8) << std::hex <<  k[977] << std::dec << std::endl;

  int j,l;

  for (j=0; j<192; j++)
    {
      os << std::dec << std::setw(5) << j << " |  ";
      for (l=0;l<5;l++) 	os << std::hex << std::setw(8) << k[9+j+l] << " " ;
      os << std::dec << std::endl;
    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}







