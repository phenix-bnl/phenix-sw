#include <packet_mutc_15_fpga.h>

Packet_mutc_15_fpga::Packet_mutc_15_fpga(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_mutc_15_fpga::decode ( int *nwout)
{
  int *k;

#define MUTCMAXLENGTH 15*128
#define MUTCMAXCHAN     128

  int *iarr = new int[MUTCMAXLENGTH];

  int dlength = getDataLength();

  int i;
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  for (i=0; i<MUTCMAXLENGTH;) iarr[i++]=0;

  decoded_data3 = new int[MUTCMAXLENGTH];
  decoded_data4 = new int[MUTCMAXLENGTH];
  decoded_data5 = new int[MUTCMAXLENGTH];


  int pos=9;
  int count=0;
  int index=0;
  int ch;

  // find all channel data
  while ( (ch = (( k[pos] >> 20) & 0xff)) <= 0x7f )
    {

      if (pos >= dlength)
	{
	  delete [] decoded_data3;
	  delete [] decoded_data4;
	  delete [] decoded_data5;
	  *nwout = 0;
	  return 0;
	}

      // the sample order in the data stream is sample  2,0,4,5,6,7,8,9,10,11,12,13,14,1,3
      // re-order them below to 0,1,2,3....

      iarr[ch*15+0] = 2047 - (k[pos +  1] & 0x7ff);
      iarr[ch*15+1] = 2047 - (k[pos + 13] & 0x7ff);
      iarr[ch*15+2] = 2047 - (k[pos + 0] & 0x7ff);
      iarr[ch*15+3] = 2047 - (k[pos + 14] & 0x7ff);

      for ( i=2; i < 13; i++) 
	{
	  iarr[ch*15 + i +2] =  2047 - (k[pos + i] & 0x7ff);
	}

      pos += 15; //advance 4 positions, that is, go to the next channel

    for ( i=0; i < 15; i++) 
      {
	decoded_data3[index+i] = ch;

	decoded_data4[index+i] = i;

	decoded_data5[index+i] = iarr[ch*15+i];
	
	index +=15;
	count++;  //increment the "emergency stop" counter, we cannot have more than 128;
	if (count >= MUTCMAXCHAN) break;

      }

    }

  data3_length = count *15;
  data4_length = count *15;
  data5_length = count *15;

  *nwout = MUTCMAXLENGTH;
  return iarr;

}

// ------------------------------------------------------

int *Packet_mutc_15_fpga::decode_misc (int *nwout)
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

  p = new int[19];

  // the order here is meant to be standard and is used below
  // 0    = detector id
  // 1    = event number
  // 2    = module address
  // 3    = flag word
  // 4    = beam clock counter
  // 5-12 = user words 0-7
  // 13   = parity word
  // 14   = dcm summary word

  p[0] = k[4] & 0xffff;  // detector id (order 4)
  p[1] = k[2] & 0xffff;  // event number (order 2)
  p[2] = k[1] & 0xffff;  // module address (order 1)
  p[3] = k[0] & 0xffff;  // flag word (order 0)
  p[4] = k[3] & 0xffff;  // beam clock counter (order 3)

  for (i=5; i< dlength; i++)
    {
      if( ( k[i] & 0x0fff0000)== 0x08080000)
	{
	  // found the userword start
	  break;
	}
    }

  if ( i == dlength) 
    {
      delete [] p;
      *nwout = 0;
      return 0;
    }

  int j;
  for (j=0; j<10; j++)   // we get 8 user words + parity + summary word, 10 words total
    p[5+j] = k[i+j];


  for (i = 0; i<4; i++) 
    {
      p[15+i] = k[5+i] & 0xffff;
    }

  *nwout = 19;
  return p;
}

// ------------------------------------------------------

int  Packet_mutc_15_fpga::iValue(const int ich, const int is)
{
  if (ich < 0 || ich > MUTCMAXCHAN) return 0;
  if (is < 0 || is > 14) return 0;

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  return decoded_data1[ich*15 + is]; 
}

// ------------------------------------------------------

int  Packet_mutc_15_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if (strcmp(what,"AMU") == 0)  // user requested AMU cells info
    {			
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      if (ich < 0 || ich >3) return 0;

      return decoded_data2[15 + ich];  // ich really refers to sample index
    }

  else if ( strcmp(what,"ID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[0];  // detector id in p array is word 0
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[1];  // word 1 in p array in event number
    }


  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      return decoded_data2[2];   // module address stored in p as word 2
    }

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      return decoded_data2[3];  // flag word stored in p as word 3
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      return decoded_data2[4];  // beam clock counter stored in p as word 4
    }

  else if ( strcmp(what,"USERWORD")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      if (ich > 7 || ich < 0) return 0;

      return decoded_data2[5+ich];  // user words are stored in p as 5-12
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      return decoded_data2[13]&0xFFFF;  // longitudinal parity word stored in p as lower 16 bits of word 13
    }

  else if ( strcmp(what,"SUMMARY")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}

      return decoded_data2[14];  // dcm summary word stored in p as word 14
    }


 else if ( strcmp(what,"CHECKPARITY")==0)
    {

      if (getHitFormat() == IDMUTC_15_FPGA0SUP ) return 0; // can't do it

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



  else	return 0;
}




// ------------------------------------------------------

void Packet_mutc_15_fpga::dump ( OSTREAM &os)
{
  int i,j;

  this->identify(os);


  // try to change the dump routine to use the iValue calls set above for consistency

  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     // Detector ID
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   // Flag Word
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;   // Beam Clock 

  os << "  AMU cell 0:         "  << SETW(8) << std::hex <<  iValue(0,"AMU") << std::dec   << std::endl; // AMU cell 0
  os << "  AMU cell 1:         "  << SETW(8) << std::hex <<  iValue(1,"AMU") << std::dec   << std::endl; // AMU cell 1
  os << "  AMU cell 2:         "  << SETW(8) << std::hex <<  iValue(2,"AMU") << std::dec   << std::endl; // AMU cell 2
  os << "  AMU cell 3:         "  << SETW(8) << std::hex <<  iValue(3,"AMU") << std::dec   << std::endl; // AMU cell 3

  for (i = 0; i<8; i++)
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  iValue(i,"USERWORD") << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(i,"PARITY") << std::dec << std::endl;
  os << "  Sumary word         " <<   SETW(8) << std::hex << iValue(i,"SUMMARY") << std::dec << std::endl;



  COUT << " channel     S0    S1    S2    S3    S4    S5    S6    S7    S8    S9    S10   S11   S12   S13   S14" <<std::endl;

  COUT << " ---------------------------------------------------------------------------------------------------" << std::endl;
 


  // find all channel data
 
  for (i=0; i< 128; i++)
    {
      os <<  SETW(5) << i << "   |  ";

      for (j=0; j< 15; j++)
	{
	  os << std::hex << SETW(5) << iValue(i,j) << " " ;
	}
      os << std::dec << std::endl;
    }
      
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;


   dumpErrorBlock(os);
   dumpDebugBlock(os);

}
//---------------------------------------------------------------------------------------------

int   Packet_mutc_15_fpga::fillIntArray (int iarr[],
				      const int nlen, int *nwout,
				      const char *what)
{
  int *from;
  int howmuch;


  *nwout = 0;

  if (strcmp(what,"") == 0)
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
  
  else if (strcmp(what,"CHN") == 0) 
    { 
      // this is no mistake, the decode fills arrays decoded_data3 as well.
      if (decoded_data1 == NULL )
        {
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    {
	      *nwout=0;
	      return -1;
	    }
	}
      howmuch = data3_length;
      from = decoded_data3;
    }

  else if (strcmp(what,"WRDCNT") == 0)
    {
      // determine what sample number this is
      // Only pass the sample numbers, we have re-ordered to be 0,1,2,3
      if (decoded_data1 == NULL )
        {
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    {
	      *nwout=0;
	      return -1;
	    }
	}
      howmuch = data4_length;
      from = decoded_data4;

    }
    
  else if (strcmp(what,"SPARSE") == 0)
    {
      // determine what sample number this is
      // Only pass the sample numbers, we have re-ordered to be 0,1,2,3
      if (decoded_data1 == NULL )
        {
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    {
	      *nwout=0;
	      return -1;
	    }
	}
      howmuch = data5_length;
      from = decoded_data5;

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






