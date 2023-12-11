

#include <packet_pc_fpga.h>

Packet_pc_fpga::Packet_pc_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
}

  
int *Packet_pc_fpga::decode (int *nwout)
{
  int *p,*k, *check;
  int i,pos;
  int dlength = getDataLength();
  int roc,word,data;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[108];
  check = new int[9];
  for (i=0;i<108; i++) p[i] = 0;
  for (i=0;i<9; i++) check[i] = 0;
  pos=5;
  while (pos <= dlength) {
    if ((k[pos] & 0x80000000) == 0x80000000) break; // indicates end of data words
    roc  = ((k[pos] & 0xff000000) >> 24);
    word = ((k[pos] & 0x00f00000) >> 20);                
    data =   k[pos] & 0x000fffff;
     if (word == 0)	 check[roc] = data;
     else if ( word <13 ) p[roc*12 + word-1] = data;
    pos++;
  }

  decoded_data2 = check;
  data2_length = 9;

  *nwout = 108;
  return p;
}


// ------------------------------------------------------


int *Packet_pc_fpga::decode_misc (int *nwout)
{

  // the order here is meant to be standard and is used below
  // 0    = detector id
  // 1    = event number
  // 2    = module address
  // 3    = flag word
  // 4    = beam clock counter
  // 5-12 = user words 0-7
  // 13   = parity word
  // 14   = dcm summary word

  int *p,*k;
  int i,pos;
  
  int dlength = getDataLength();
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  
  p = new int[15];
  
  p[0] = k[0] & 0xfffff;    // Det id
  p[1] = k[1] & 0xfffff;    // Event number
  p[2] = k[2] & 0xfffff;    // Module address
  p[3] = k[3] & 0xfffff;    // flag word
  p[4] = k[4] & 0xfffff;    // Beam Clock Counter
  
  pos=5;
  while (pos < dlength)
    {
      if ( (k[pos] & 0xff000000) == 0x81000000) break;
      pos++;
    }
  
  // found 1st userword
  
  for (i = 0; i<10; i++)
    p[5+i] = k[pos+i] & 0xfffff;
  
  
  *nwout = 15;
  return p;
}

// ------------------------------------------------------

int  Packet_pc_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if ( strcmp(what,"DWORD")==0)
    {
      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}
      if (ich < 0 || ich >= data1_length) return 0;
      return decoded_data1[ich];
    }

  else if ( strcmp(what,"CHECKCTR")==0)
    {
      if (ich < 0 || ich > 9) return 0;
      if (decoded_data2 == NULL )
	{
	  // data1 and data2 are decoded in the same routine, so we 
	  // decode data1 here. 
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}
	
      return decoded_data2[ich];
    }
  
  else if ( strcmp(what,"PAD")==0)
    {
      return iValue(ich);
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
  
  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
      return decoded_data3[3];
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

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
       return decoded_data3[13];
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



  //se if ( strcmp(what,"MISMATCH")==0)
  //{
  //  int l;
  //  int * k = (int *) findPacketDataStart(packet);
  //  int fval = k[5] & 0xfffff; // the first check value 
  //  for (l = 1; l< 9; l++)
  //	{
  //	  if ( fval != (   k[5+l*13 ] & 0xfffff ) )
  //	    return (9-l);
  //	}
  //  return 0;
  //}


  else return 0;

}


// ------------------------------------------------------

int Packet_pc_fpga::iValue(const int ich)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
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


// ------------------------------------------------------

void Packet_pc_fpga::dump ( OSTREAM &os) 
{

  int i;

  this->identify(os);


  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;  // Beam Clock Counter

  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  iValue(i,"USERWORD") << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(i,"PARITY") << std::dec << std::endl;

  int j,l;
  COUT << "  roc      0     1     2     3     4     5     6     7     8     9     10    11    check" << std::endl;
  COUT << " ---------------------------------------------------------------------------------------" << std::endl; 

  for (j=0; j<9; j++)
    {
      os << std::dec << SETW(5) << 9-j << " |  ";
      for (l=0;l<12;l++) 	os << std::hex << SETW(5) << iValue(j*12 + l,"DWORD") << " " ;
      os << " " << SETW(5) << iValue(j,"CHECKCTR") << std::dec << std::endl;
    }
	
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;

  dumpErrorBlock(os);
  dumpDebugBlock(os);
}


