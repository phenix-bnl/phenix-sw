#include <packet_emc_fpga.h>

Packet_emc_fpga::Packet_emc_fpga(PACKET_ptr data)
  : Packet_emc (data)
{
  max_channels=144;
  hitlength = 0;
}
  
int *Packet_emc_fpga::decode ( int *nwout)
{


  int *k;
  int temp[MAX_OUTLENGTH];
  memset(temp,0,MAX_OUTLENGTH*sizeof(int));

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }


  int dlength = getDataLength();

  int pos=8;
  int ch;

  // find all channel data
  int olength = 0;
  int currentmaxlen;
  while (pos <= dlength) 
    {

      if ((k[pos] & 0x10000000) == 0x10000000) break; // indicates end of data words

      ch = (k[pos] >> 20 ) & 0xff;
      currentmaxlen = ch*5+4+1; // maximum array index for current channel
      if (currentmaxlen >= MAX_OUTLENGTH)
	{
          *nwout = 0;
          return 0;
	}
      if (currentmaxlen > olength)
	{
	  olength = currentmaxlen;
	}

      switch ( ( (k[pos] >> 16 ) & 0xf))
	{
	case 0x9:  // high gain post
	  temp[ch*5+1] = k[pos] & 0xfff;
	  break;

	case 0xa:  // high gain pre
	  temp[ch*5+3] = k[pos] & 0xfff;
	  break;

	case 0xb:  // low gain post
	  temp[ch*5+2] = k[pos] & 0xfff;
	  break;

	case 0xc:  // low gain pre 
	  temp[ch*5+4] = k[pos] & 0xfff;
	  break;

	case 0xd:  // timing sample
	  temp[ch*5+0] = k[pos] & 0xfff;
	  hitlist [hitlength++] = ch;
	  break;

	default:
	  break;
	}
      pos++;
    }

  int *p = new int[olength];
  int *kk = p;
  for (int i =0; i<olength; i++) 
    {
      *kk++ = temp[i];
    }
  *nwout = olength;
  return p;
}

int Packet_emc_fpga::decode_to_sparse ( int *p, const int nlen )
{
  int index = 0;
  int i;
  int count = 0;

  // trigger the decoding
  i =    iValue(0,0);

  for ( i = 0; i < hitlength; i++)
    {

      if ( index+6 > nlen )
	{
	  std::cout << __FILE__ << " " << __LINE__<< ": Receiving array too small - " << nlen << std::endl;
	  return 0;
	}
      p[index+0] = hitlist[i];
      p[index+1] = iValue(hitlist[i],0);
      p[index+2] = iValue(hitlist[i],1);
      p[index+3] = iValue(hitlist[i],2);
      p[index+4] = iValue(hitlist[i],3);
      p[index+5] = iValue(hitlist[i],4);

      count++;
      index +=6;
    }

  return count;
}

// ------------------------------------------------------

int *Packet_emc_fpga::decode_amu (int *nwout)
{
  int *p,*k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[3];

  //output is supposed to be
  // pre - post - timing
  // but here the data are post - pre - timing
  
  p[0] = k[7] & 0x3f;  // time
  p[1] = k[6] & 0x3f;  // pre
  p[2] = k[5] & 0x3f;  // post
 

  *nwout = 3;
  return p;
}

// ------------------------------------------------------

int *Packet_emc_fpga::decode_misc (int *nwout)
{
  int *p, *k;

  int dlength = getDataLength();
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[7];

  // the order here is meant to be standard and is used below
  // 0    = detector id
  // 1    = event number
  // 2    = module address
  // 3    = flag word
  // 4    = beam clock counter
  // 5   = parity word
  // 6   = dcm summary word


  p[0] = k[4] & 0xffff;  // detector id (order 4)
  p[1] = k[2] & 0xffff;  // event number (order 2)
  p[2] = k[1] & 0xffff;  // module address (order 1)
  p[3] = k[0] & 0xffff;  // flag word (order 0)
  p[4] = k[3] & 0xffff;  // beam clock counter (order 3)

  p[5] = k[dlength-2] & 0xffff;  // parity
  p[6] = k[dlength-1] & 0xffff;  // status

  *nwout = 7;
  return p;
}

// ------------------------------------------------------

int  Packet_emc_fpga::iValue(const int ich, const char *what)
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

  else if ( strcmp(what,"ID")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[0];  // detector id in p array is word 0
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[1];  // word 1 in p array in event number
    }


  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[2];   // module address stored in p as word 2
    }

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[3];  // flag word stored in p as word 3
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[4];  // beam clock counter stored in p as word 4
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[5];  // longitudinal parity word stored in p as word 5
    }

  else if ( strcmp(what,"SUMMARY")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[6];  // dcm summary word stored in p as word 6
    }


  else	return 0;
}


// ------------------------------------------------------

int   Packet_emc_fpga::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	{
	  return 0;
	}
    }

  // see if our array is long enough and return channel
  if ((ich*5 + iy) < data1_length)
    {
      return decoded_data1[ich*5 + iy];
    }
  return 0;
}

// ------------------------------------------------------

void Packet_emc_fpga::dump ( OSTREAM &os) 
{

  int i,j;

  this->identify(os);

  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     // Detector ID
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   // Flag Word
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;   // Beam Clock 

  os << "  AMU cell 0:         "  << SETW(8) << std::hex <<  iValue(0,"AMU") << std::dec   << std::endl; // AMU cell 0
  os << "  AMU cell 1:         "  << SETW(8) << std::hex <<  iValue(1,"AMU") << std::dec   << std::endl; // AMU cell 1
  os << "  AMU cell 2:         "  << SETW(8) << std::hex <<  iValue(2,"AMU") << std::dec   << std::endl; // AMU cell 2

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(0,"PARITY") << std::dec << std::endl;
  os << "  Sumary word         " <<   SETW(8) << std::hex << iValue(0,"SUMMARY") << std::dec << std::endl;



  os << " channel      time high-post low-post high-pre low-pre" <<std::endl;
  os << " -----------------------------------------------------" << std::endl;


  // find all channel data
 
  for (i=0; i< 144; i++)
    {
      os <<  SETW(5) << i << " |  ";

      for (j=0; j< 5; j++)
	{
	  os << std::hex << SETW(8) << iValue(i,j) << " " ;
	}
      os << std::dec << std::endl;
    }
      


   dumpErrorBlock(os);
   dumpDebugBlock(os);
}

int   Packet_emc_fpga::fillIntArray (int iarr[],
				      const int nlen, int *nwout,
				      const char *what)
{
  int *from;
  int howmuch;

  *nwout=0;

  // the fast trigger routine 
  if (strcmp(what,"SPARSE") == 0)
    {
      int uu =  decode_to_sparse(iarr,nlen);
      *nwout = 6*uu;
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
/* $$$$$$$$$$$$$ functions for new suppressed/nonsupressed format data $$$$$$$$$$$$$ */

int Packet_emc_fpga::filliList5x144(int** rawData, int* address,  int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{
  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{
	  *address++=channel+addressOffset;
	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  nch++;
	}

    }
  return nch;
}

int Packet_emc_fpga::fillfList5x144(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{
	  *address++=channel+addressOffset;
	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  nch++;
	}

    }
  return nch;

}

int Packet_emc_fpga::filliList6x144(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  rawData[5][nch+arrayOffset] = channel+addressOffset;
	  nch++;
	}
    }
  return nch;
}


int Packet_emc_fpga::filliList144x6(int* rawData, int addressOffset=0, int threshold=-4096)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  *rawData++ = iValue(channel,0);
	  *rawData++ =  iValue(channel,1);
	  *rawData++ =  iValue(channel,2);
	  *rawData++ =  iValue(channel,3);
	  *rawData++ =  iValue(channel,4);
	  *rawData++ = channel+addressOffset;
	  nch++;
	}
    }
  return nch;
}


int Packet_emc_fpga::fillArray144x5(int* rawData)
{
  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  *rawData++ = iValue(channel,0);
	  *rawData++ =  iValue(channel,1);
	  *rawData++ =  iValue(channel,2);
	  *rawData++ =  iValue(channel,3);
	  *rawData++ =  iValue(channel,4);
	  nch++;
	}
    }
  return nch;

}

int Packet_emc_fpga::filliList5x192(int** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 192 )
	{
	  *address++=channel+addressOffset;
	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  nch++;
	}
    }
  return nch;

}


int Packet_emc_fpga::fillfList5x192(float** rawData, int* address, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{


  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{
	  *address++=channel+addressOffset;
	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  nch++;
	}

    }
  return nch;

}

int Packet_emc_fpga::filliList6x192(int** rawData, int arrayOffset=0, int addressOffset=0, int threshold=-4096)
{


  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 192 )
	{

	  rawData[0][nch+arrayOffset] = iValue(channel,0);
	  rawData[1][nch+arrayOffset] =  iValue(channel,1);
	  rawData[2][nch+arrayOffset] =  iValue(channel,2);
	  rawData[3][nch+arrayOffset] =  iValue(channel,3);
	  rawData[4][nch+arrayOffset] =  iValue(channel,4);
	  rawData[5][nch+arrayOffset] = channel+addressOffset;
	  nch++;
	}
    }
  return nch;

}


int Packet_emc_fpga::filliList192x6(int* rawData, int addressOffset=0, int threshold=-4096)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 192 )
	{

	  *rawData++ = iValue(channel,0);
	  *rawData++ =  iValue(channel,1);
	  *rawData++ =  iValue(channel,2);
	  *rawData++ =  iValue(channel,3);
	  *rawData++ =  iValue(channel,4);
	  *rawData++ = channel+addressOffset;
	  nch++;
	}
    }
  return nch;

}


int Packet_emc_fpga::fillArray192x5(int* rawData)
{

  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 192 )
	{

	  *rawData++ = iValue(channel,0);
	  *rawData++ =  iValue(channel,1);
	  *rawData++ =  iValue(channel,2);
	  *rawData++ =  iValue(channel,3);
	  *rawData++ =  iValue(channel,4);
	  nch++;
	}
    }
  return nch;


}
int Packet_emc_fpga::fillCoarseEnergyList(int* Energy, int* address, int addressOffset=0, int threshold=-4096)
{


  int i,channel, nch;
  i =    iValue(0,0);

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  int post;

	  *address++=channel+addressOffset;
	  post= iValue (channel, 1) ;
	  if(post>MAX_HIGH_GAIN) // low gain
	    {
	      *Energy++ = ( iValue(channel,2)  - iValue(channel,4)  )<<4;
	    }
	  else // high gain
	    {
	      *Energy++ =post - iValue(channel,3);
	    }
	  nch++;
	}
    }
  return nch;
}


int Packet_emc_fpga::fillCoarseEnergyArray(int* Energy)
{

  int i,channel, nch;
  i =    iValue(0,0);

  memset(Energy,0,144*sizeof(int));

  nch=0;
  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  int post;

	  post= iValue (channel, 1) ;
	  if(post>MAX_HIGH_GAIN) // low gain
	    {
	      Energy[channel] = ( iValue(channel,2)  - iValue(channel,4)  )<<4;
	    }
	  else // high gain
	    {
	      Energy[channel] =post - iValue(channel,3);
	    }
	  nch++;
	}
    }
  return nch;



}

int Packet_emc_fpga::fillQtileArray(int* qtileEnergy)
{

  int i,channel, nch;
  i =    iValue(0,0);

  memset(qtileEnergy,0,36*sizeof(int));

  nch=0;

  for (i=0; i < hitlength ; i++)
    {
      channel= hitlist[i];
      if(channel >= 0 && channel < 144 )
	{

	  int post;

	  post= iValue (channel, 1) ;
	  if(post>MAX_HIGH_GAIN) // low gain
	    {
	      qtileEnergy[channel<<2 ]   = ( iValue(channel,2)  - iValue(channel,4)  )<<4;
	    }
	  else // high gain
	    {
	       qtileEnergy[channel<<2 ]  =post - iValue(channel,3);
	    }
	  nch++;
	}
    }
  return nch/4;

}

int Packet_emc_fpga::fillQtileList(int* qtileEnergy, int* address, int addressOffset=0, int threshold=0)
{
  int i, post, tile, prevtile;
  int channel, nch;
  i =    iValue(0,0);

  nch = 0;
  *qtileEnergy=0;
  i = iValue(0,0); // trigger the decoding 

  if (! hitlength) return 0;

  prevtile= hitlist[0]/4;
  channel =  hitlist[0];

  if( prevtile > 35 ) return 0;
  *address = prevtile + addressOffset;

  for (i=0; i < hitlength ; i++)
    {
      channel = hitlist[i];
      tile= channel / 4;
      if(tile < 36 )
	{
	  post= iValue (channel, 1) ;
	  if(tile!=prevtile)
	    {
	      prevtile=tile;
	      if(*qtileEnergy > threshold) {
		qtileEnergy++;
		address++;
		nch++;
	      }
	      *qtileEnergy=0;
	      *address=prevtile + addressOffset;
	    }
	  
	  post= iValue (channel, 1) ;
	  if(post>MAX_HIGH_GAIN) // low gain
	    {
	      *qtileEnergy += ( iValue(channel,2)  - iValue(channel,4)  )<<4;
	    }
	  else // high gain
	    {
	      *qtileEnergy += post - iValue(channel,3);
	    }

	}
      
    }
  if( nch >0 && *qtileEnergy <= threshold) 
    {
      nch--;
    }
  return nch;
}




