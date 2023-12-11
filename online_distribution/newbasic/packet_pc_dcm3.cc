#include <packet_pc_dcm3.h>

Packet_pc_dcm3::Packet_pc_dcm3(PACKET_ptr data)
  : Packet_w4 (data)
{
}

  
int *Packet_pc_dcm3::decode ( int *nwout)
{

  return decode_pad (nwout);

}
// ------------------------------------------------------

int *Packet_pc_dcm3::decode_pad (int *nwout)
{
  int *p,*k;
  int j;
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
  for (int i = 0; i<9; i++)
    {
      for (j = 1; j<13; j++)
	p[i*12 + j-1] = k[5+ i*13 + j] & 0xfffff;
    }
  *nwout = 108;
  return p;
}


// ------------------------------------------------------


int *Packet_pc_dcm3::decode_misc (int *nwout)
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
    p[5+i] = k[5+117+i] &0xfffff;
  
  
  *nwout = 14;
  return p;
}


// ------------------------------------------------------

int  Packet_pc_dcm3::iValue(const int ich)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode_pad(&data1_length))==NULL)
	return 0;
    }
      
  int ipword = ich/20;
  int iword = 107 - ipword;
  int ibit  = 19- (ich%20); // turn the bits  around
      
  if (iword >= data1_length) return 0;
      
  int ival  = decoded_data1[iword];
  int ixx  = ( ival >> ibit) & 0x1;
  return ixx;

}


int  Packet_pc_dcm3::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if ( strcmp(what,"PAD")==0)
    {

      return iValue(ich);

    }

  else if ( strcmp(what,"DWORD")==0)
    {
      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode_pad(&data1_length))==NULL)
	    return 0;
	}
      if (ich < 0 || ich >= data1_length) return 0;
      return decoded_data1[ich];
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

  else if ( strcmp(what,"CHECKCTR")==0)
    {
      if (ich < 0 || ich > 9) return 0;
      int * k = (int *) findPacketDataStart(packet);
      if (k == 0) 
	{
	  return 0;
	}

      return k[5+ ich*13 ] & 0xfffff;
      
    }

  else if ( strcmp(what,"PADSUM")==0)
    {
      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}
      int nbitson = 0;
      int iword,j;
      for (iword=0; iword<108; iword++)
	{
	  for (j=0; j<20; j++) 
	    {
	      if ( (decoded_data1[iword] >> j) & 0x1 ) // if true then bit is on
		  nbitson++;
	    }
	}    
      return nbitson;
    }


  else if ( strcmp(what,"MISMATCH")==0)
    {
      int l;
      int * k = (int *) findPacketDataStart(packet);
      if (k == 0) 
	{
	  return 0;
	}


      int fval = k[5] & 0xfffff; // the first check value 
      for (l = 1; l< 9; l++)
	{
	  if ( fval != (   k[5+l*13 ] & 0xfffff ) )
	    return (9-l);
	}
      return 0;
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      int * k = (int *) findPacketDataStart(packet);
      if (k == 0) 
	{
	  return 0;
	}

      return  k[5+117+8];
    }

 else if ( strcmp(what,"CHECKPARITY")==0)
    {

      int my_parity = 0;
      int *k = (int *) findPacketDataStart(packet);
      if ( ! k) return 0;
      int j=0;
      while (1)
	{
	  my_parity ^= (k[j++] & 0xFFFFF);
	  if ( j>= getDataLength()-2 ) break;
	}
      if ( my_parity != iValue(0,"PARITY")) return -1;
      else return 1;
      
    }



  else return 0;

}


// ------------------------------------------------------

void Packet_pc_dcm3::dump ( OSTREAM &os) 
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


  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[1]  & 0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex <<  k[2] << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex <<  k[3] << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[4] & 0xfffff) << std::dec << std::endl;    // Beam Clock Counter

  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[5+117+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[5+117+8] << std::dec << std::endl;

  int j,l;

  for (j=0; j<117; j+=13)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<13;l++) 	os << std::hex << SETW(6) << (k[5+j+l] & 0xfffff) << " " ;
      os << std::dec << std::endl;
    }
	
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;

  dumpErrorBlock(os);
  dumpDebugBlock(os);
}


