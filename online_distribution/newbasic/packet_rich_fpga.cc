#include <packet_rich_fpga.h>

Packet_rich_fpga::Packet_rich_fpga(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_rich_fpga::decode ( int *nwout)
{
  int *p,*k;
  int pos;
  int dlength = getDataLength();
  int sw,word,sample,data;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[3*160];

  //  for (i=0;i<3*160; i++) p[i] = 0;
  memset (p, 0, 3*160*sizeof(int));

  pos=8;
  while (pos <= dlength) {
    if ( (k[pos] & 0x0a000000) != 0x0a000000) 
      {
	word = ( (k[pos] & 0x0Ff00000) >> 20);	
	sw = ( k[pos] & 0x70000 ) >> 16;
	if (sw ==1) sample = 0;     // time sample 0x9 &7    = 1
	else if (sw ==2) sample =1; // post sample 0xa &7    = 2
	else if (sw ==4) sample =2; // pre sample  0xc &7    = 4
        else 
	  {
	    std::cout << "packet_rich_fpga::std::decode bad sw value: " << sw << std::endl;
	    *nwout = 0;
            delete [] p;
	    return NULL;
	  }
	data =   k[pos] & 0x000003ff;
	if (word >= 0 && word < 160 ) p[3*word+sample] = data;
      }
    else
      {
	break;
      }
    pos++;
  }

  *nwout = 3*160;
  return p;

}


int Packet_rich_fpga::iValue(const int ich)
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



int *Packet_rich_fpga::decode_misc (int *nwout)
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

  p = new int[18];

  p[0] = k[4] & 0xffff;    // Det id
  p[1] = k[2] & 0xffff;    // Event number
  p[2] = k[1] & 0xffff;    // Module address
  p[3] = k[0] & 0xffff;    // flag word
  p[4] = k[3] & 0xffff;    // Beam Clock Counter
  
  pos=8;
  while (pos < dlength)
    {
      if ( (k[pos] & 0x0a080000) == 0x0a080000) break; 
      pos++;
    }

  // found 1st userword
  
  // these are the userwords 0 through 6. 
  for (i = 0; i<8; i++) p[5+i] = ( k[pos+i] & 0xffff); 
  
  // number 7 is different, it gets moved to position 1 
  p[13] = k[pos+8] & 0xffff; // long. parity
  p[14] = k[pos+9] & 0xffff; // summary word

  p[15] = k[5] & 0xff;
  p[16] = k[6] & 0xff;
  p[17] = k[7] & 0xff;
  
  
  *nwout = 18;
  return p;
}


int  Packet_rich_fpga::iValue(const int ich, const int iy)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
  if (ich < 0 || ich >= 160) return 0;
  if (iy < 0 || iy >= 3) return 0;
  return decoded_data1[ich*3 + iy];
  
}

int  Packet_rich_fpga::iValue(const int ich, const char *what)
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
       if (ich < 0 || ich >2 ) return 0;
       return decoded_data3[15+ich];
    }

 else if ( strcmp(what,"CHECKPARITY")==0)
    {

      if (getHitFormat() == IDRICH_FPGA0SUP ) return 0; // can't do it

      int my_parity = 0;
      int *k = (int *) findPacketDataStart(packet);
      if ( ! k) return 0;
      int j=0;
      while (1)
	{
	  my_parity ^= (k[j++] & 0xFFFF);
	  if ( j>= getDataLength()-2 ) break;
	}
      if ( my_parity != iValue(0,"PARITY")) return -1;
      else return 1;
      
    }



  else return 0;

}


int   Packet_rich_fpga::fillIntArray (int iarr[],
				      const int nlen, int *nwout,
				      const char *what)
{
  int *from;
  int howmuch;

  *nwout=0;


  // the fast trigger routine 
  if (strcmp(what,"SPARSE") == 0)
    {
      int uu =  decode_to_sparse(iarr);
      *nwout = 4*uu;
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
  if (howmuch < 0) return -3;

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



int Packet_rich_fpga::decode_to_sparse (int *p)
{
  int *k;
  int pos,ii;
  int dlength = getDataLength();
  int sw,word,data;
  int count = 0;

  struct richChannelList *l = ( struct richChannelList *) p;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }


  pos=8;
  while (pos <= dlength) {
    if ( (k[pos] & 0x0a000000) != 0x0a000000) 
      {
	word = ( (k[pos] & 0x0Ff00000) >> 20);
	for (ii = 0; ii < 3; ii++)
	  {
	    sw = ( k[pos+ii] & 0x70000 ) >> 16;
	    data =   k[pos+ii] & 0x000003ff;
	    l->channel = word;
	    if (sw ==1) l->time = data;
	    else if (sw ==2)l->post = data;
	    else if (sw ==4) l->pre = data;
	  }
	count++;
	l++;

      }
    else
      {
	break;
      }
    pos+=3;
  }
  return count;
}



void Packet_rich_fpga::dump ( OSTREAM &os) 
{
  int i;

  this->identify(os);


  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;  // Beam Clock Counter

  for (i = 0; i<3; i++)
    os << " AMU       " << i << "  " <<   SETW(8) << std::hex <<  iValue(i,"AMU") << std::dec << std::endl;


  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  iValue(i,"USERWORD") << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(0,"PARITY") << std::dec << std::endl;
  os << "  DCM Status word     " <<   SETW(8) << std::hex << iValue(0,"STATUS") << std::dec << std::endl;

  int j,l;
  COUT << "  index      post pre time " << std::endl;
  COUT << " --------------------------" << std::endl; 

  for (j=0; j<160; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<3;l++) 	os << std::hex << SETW(5) << iValue(j, l) << " " ;
      os << std::endl;

    }
  
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;
  
  
  dumpErrorBlock(os);
  dumpDebugBlock(os);
}


