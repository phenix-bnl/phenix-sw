#include <packet_mvd_fpga.h>

Packet_mvd_fpga::Packet_mvd_fpga(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_mvd_fpga::decode ( int *nwout)
{
  int *p,*k;
  int i,pos;
  int dlength = getDataLength();
  int word,data;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[256];
  for (i=0;i<256; i++) p[i] = 0;

  pos=7;
  while (pos <= dlength) {
    if ( (k[pos] & 0x000c0000) == 0x000c0000) 
      {
	word = ((k[pos] & 0x0ff00000) >> 20);                
	data =   k[pos] & 0x0000ffff;
      }
    if (word >= 0 && word < 256 ) p[word] = data;
    pos++;
  }

  *nwout = 256;
  return p;

}


int Packet_mvd_fpga::iValue(const int ich)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  if (ich >= data1_length) return 0;

  return decoded_data1[ich];
}



int *Packet_mvd_fpga::decode_misc (int *nwout)
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

  p = new int[17];

  p[0] = k[4] & 0xffff;    // Det id
  p[1] = k[2] & 0xffff;    // Event number
  p[2] = k[5] & 0xffff;    // Module address
  p[3] = k[0] & 0xffff;    // flag word
  p[4] = k[3] & 0xffff;    // Beam Clock Counter
  
  pos=7;
  while (pos < dlength)
    {
      if ( (k[pos] & 0x10080000) == 0x10080000) break; 
      pos++;
    }

  // found 1st userword
  
  // these are the userwords 0 through 6. 
  for (i = 0; i<7; i++) p[5+i] = ( k[pos+i] & 0xffff); 
  
  // number 7 is different, it gets moved to position 1 
  p[5+7] =  k[1]   & 0xffff;
  p[13] = k[pos+7] & 0xffff;
  p[14] = k[pos+8] & 0xffff;
  
  // this is the "amu cell no"
  p[15] = k[6] & 0x3f;
  p[16] = (k[6] >> 8) & 0x3f;

  *nwout = 17;
  return p;
}


int  Packet_mvd_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if ( strcmp(what,"ID")==0)
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

  else if ( strcmp(what,"STATUS")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
       return decoded_data3[14];
    }

  else if ( strcmp(what,"AMU")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      
      if (ich > 1 || ich < 0) return 0;
      return decoded_data3[15+ich];
    }
  
  

  else return 0;

}


void Packet_mvd_fpga::dump ( OSTREAM &os) 
{
  int i;

  this->identify(os);


  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;  // Beam Clock Counter
  os << "  AMU Cell Nrs :      "  << SETW(8) << std::hex << iValue(0,"AMU") << SETW(8) << iValue(1,"AMU")<< std::dec << std::endl;  // AMU cell number

  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  iValue(i,"USERWORD") << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(0,"PARITY") << std::dec << std::endl;
  os << "  DCM Status word     " <<   SETW(8) << std::hex << iValue(0,"STATUS") << std::dec << std::endl;

  int j,l;
  COUT << "  index      0     1     2     3     4     5     6     7     8     " << std::endl;
  COUT << " ------------------------------------------------------------------" << std::endl; 

  for (j=0; j<32; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<8;l++) 	os << std::hex << SETW(5) << iValue(j*8 + l) << " " ;
      os << std::endl;

    }
	

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}


