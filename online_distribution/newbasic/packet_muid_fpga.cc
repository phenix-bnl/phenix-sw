

#include <packet_muid_fpga.h>

Packet_muid_fpga::Packet_muid_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
}

  
int *Packet_muid_fpga::decode (int *nwout)
{
  int *p,*k;
  int pos;
  int dlength = getDataLength();
  int roc,word,data;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[120];
  memset (p, 0, 120*sizeof(int) );


  pos=5;
  while (pos <= dlength) 
    {
      if ((k[pos] & 0x80000000) == 0x80000000) break; // indicates end of data words
      roc  = ((k[pos] & 0xff000000) >> 24);
      word = ((k[pos] & 0x00700000) >> 20);                
      data =   k[pos] & 0x000fffff;
      if ( word <6  && roc < 20 ) p[roc*6 + word] = data;
      pos++;
    }

  *nwout = 120;
  return p;
}


// ------------------------------------------------------


int *Packet_muid_fpga::decode_misc (int *nwout)
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

int  Packet_muid_fpga::iValue(const int ich, const char *what)
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

int Packet_muid_fpga::iValue(const int ich)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
      
  if (ich >=0 && ich < 120) return decoded_data1[ich];

  return 0;

}

// ------------------------------------------------------

int Packet_muid_fpga::iValue(const int ich, const int roc)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
      
  if ( ich < 0 || ich > 5 || roc < 0 || roc > 19 ) return 0;

  return (decoded_data1[roc*6 + ich] & 0xffff );

  return 0;
  
}
// ----------------------------------------------

int
Packet_muid_fpga::fillIntArray (int iarr[],
			const int nlen, int *nwout,
			const char *what)
{

  int *k;
  int i,pos;
  int dlength = getDataLength();

  int *from;
  int howmuch;

  *nwout = 0;


  if (strcmp(what,"SPARSE") == 0)
    {
      k = (int *) findPacketDataStart(packet);
      if (k == 0) 
	{
	  *nwout = 0;
	  return 0;
	}
      pos=5;
      i = 0;
      while (pos <= dlength) 
	{
	  if ((k[pos] & 0x80000000) == 0x80000000) break; // indicates end of data words
	  iarr[i++] = k[pos];
	  pos++;
	}

      *nwout = i;
      return 0;
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
      howmuch = getLength();
      from = (int *) packet;
    }

  else if (strcmp(what,"DATA") == 0)
    {
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

  if (from == 0 )
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


// ------------------------------------------------------

void Packet_muid_fpga::dump ( OSTREAM &os) 
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
  os << "  Sumary word         " <<   SETW(8) << std::hex << iValue(i,"SUMMARY") << std::dec << std::endl;

  int j,l;
  COUT << "  roc      0     1     2     3     4     5    " << std::endl;
  COUT << " ---------------------------------------------" << std::endl; 

  for (j=0; j<20; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<6;l++) 	os << std::hex << SETW(5) << iValue(j*6 + l) << " " ;
      os << std::endl;
    }
	
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;
  

   dumpErrorBlock(os);
   dumpDebugBlock(os);
}


