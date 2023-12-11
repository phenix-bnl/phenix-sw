#include <packet_pc_dcm0.h>

Packet_pc_dcm0::Packet_pc_dcm0(PACKET_ptr data)
  : Packet_w4 (data)
{
}

  
int *Packet_pc_dcm0::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int i;
  int temp[MAX_OUTLENGTH];

  int dlength = getDataLength();

  int status = decode_pc_dcm0( temp
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

int *Packet_pc_dcm0::decode_pad (int *nwout)
{
  int *p,*k;
  
  int dlength = getDataLength();

  if ( dlength < 122 ) 
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

  p = new int[108];
  for (int i = 0; i<108; i++)
    p[i] = k[5+i] & 0xfffff;
  *nwout = 108;
  return p;
}


// ------------------------------------------------------

int *Packet_pc_dcm0::decode_misc (int *nwout)
{
  int *p,*k;
  int i;
  
  int dlength = getDataLength();
  if ( dlength < 122 ) 
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
  
  p = new int[14];
  
  p[0] = k[0] & 0xfffff;    // Det id
  p[1] = k[1] & 0xfffff;    // Event number
  p[2] = k[2] & 0xfffff;    // Module address
  p[3] = k[3] & 0xfffff;    // flag word
  p[4] = k[4] & 0xfffff;    // Beam Clock Counter
  
  
  for (i = 0; i<9; i++)
    p[5+i] = k[113+i] & 0xfffff;
  
  
  *nwout = 14;
  return p;
}

// ------------------------------------------------------

int  Packet_pc_dcm0::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if ( strcmp(what,"PAD")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_pad(&data2_length))==NULL)
	    return 0;
	}
      int ipword = ich/20;
      int iword = 107 - ipword;
      int ibit  = ich%20;
      int realbit =    ibit +3 -2 * (ibit%4);


      if (iword >= data2_length) return 0;
      
      int ival  = decoded_data2[iword];
      int ixx  = ( ival >> realbit) & 0x1;
      return ixx;
    }

  else if ( strcmp(what,"ID")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[0];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[1];
    }

  
  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
      return decoded_data3[2];
    }
  
  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
      return decoded_data3[4];
    }
  
  
  else if ( strcmp(what,"USERWORD")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
      if (ich > 7 || ich < 0) return 0;
      
      return decoded_data3[5+ich];
    }
  else return 0;

}


// ------------------------------------------------------

void Packet_pc_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  if ( dlength < 120 ) return ;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }


  os << "  Start marker word : "  << SETW(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[1] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[2]  & 0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex <<  k[3] << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex <<  k[4] << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[5] & 0xfffff) << std::dec << std::endl;    // Beam Clock Counter

  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[6+108+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[6+108+8] << std::dec << std::endl;

  int j,l;

  for (j=0; j<108; j+=4)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<4;l++) 	os << std::hex << SETW(8) << k[6+j+l] << " " ;
      os << std::dec << std::endl;
    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}


