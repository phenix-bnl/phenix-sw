//modefied for 2023 words output format  ML
#include <packet_mutc_dcm0.h>

#include  <string.h>

Packet_mutc_dcm0::Packet_mutc_dcm0(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_mutc_dcm0::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_mutc_dcm0( temp
				 ,(int *)  findPacketDataStart(packet) 
				 ,dlength
				 ,MAX_OUTLENGTH, &olength);

  if (status || olength<=0 ) return NULL;
 
  p = new int[olength];
  k = p;
  for (i =0; i<olength; i++) *k++ = temp[i];
  *nwout = olength;
  return p;
}

// ------------------------------------------------------
// get mutr AMU cell addresses
int *Packet_mutc_dcm0::decode_amu (int *nwout)
{
  int *p,*k;

  int dlength = getDataLength();
  if ( dlength < 2023 ) 
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
  p = new int[4];
  for (int i = 0; i<4; i++)
    p[i] = k[6+i] & 0x3f;
  *nwout = 4;
  return p;
}

// ------------------------------------------------------
// get header info 
int *Packet_mutc_dcm0::decode_misc (int *nwout)
{
  int *p,*k;
  int i;

  int dlength = getDataLength();
  if ( dlength < 2023 ) 
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

  p[0] = k[2] & 0xffff;  // Event number
  p[1] = k[3];           // Module Address
  p[2] = k[5] & 0xffff;    // Beam Clock Counter


  for (i = 0; i<9; i++)  // 8 user words and one parity words
    p[3+i] = k[2013+i];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

int  Packet_mutc_dcm0::iValue(const int ich, const char *what)
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

      if (ich >= data2_length) return 0;

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

int   Packet_mutc_dcm0::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  //  if (ich >= 128) return 0;
  //  if (iy >= 4) return 0;
  //  return decoded_data1[ich + iy*128];

  if (ich >= 32) return 0;   // 32 channles for cosmic FPGA
  if (iy >= 63) return 0;   // missed one word in FPGA
  
  //  return decoded_data1[ich + iy*128];
  return decoded_data1[ich + iy*32];
}

// ------------------------------------------------------

void Packet_mutc_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  if ( dlength < 2023 ) return ;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }

  os << "  Start marker word : "  << SETW(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[1] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[2]  & 0xffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex <<  k[3] << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex <<  k[4] << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[5] & 0xffff) << std::dec << std::endl;    // Beam Clock Counter
  os << "  AMU cell 1:         "  << SETW(8) << std::hex << (k[6] & 0x3f) << std::dec   << std::endl;    // AMU cell 0
  os << "  AMU cell 2:         "  << SETW(8) << std::hex << (k[7] & 0x3f) << std::dec   << std::endl;    // AMU cell 1
  os << "  AMU cell 3:         "  << SETW(8) << std::hex << (k[8] & 0x3f) << std::dec   << std::endl;    // AMU cell 2
  os << "  AMU cell 4:         "  << SETW(8) << std::hex << (k[9] & 0x3f) << std::dec   << std::endl;    // AMU cell 3


  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[2013+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[2021] << std::dec << std::endl;

  int j,l;

  //  for (j=0; j<128; j++)
  
  for (j=0; j<32; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      //      for (l=0;l<4;l++) 	os << std::hex << SETW(8) << k[10+j+ 128*l] << " " ;
      for (l=0;l<63;l++) 	os << std::hex << SETW(8) << k[5+j+ 32*l] << " " ; // start from k[5]
      os << std::dec << std::endl;
    }
	
   dumpErrorBlock(os);
   dumpDebugBlock(os);


}


