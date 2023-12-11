//  Packet class to read real data with the following format:
//    0xffff  		First word
//    5 user words
//    4 amu samples
//    channel 0, sample 0
//    channel 1, sample 0
//    ...
//    channel 127, sample 0
//    channel 0, sample 1
//    channel 1, sample 1
//    ...
//    channel 127, sample 1
//    channel 0, sample 2
//    channel 1, sample 2
//    ...
//    channel 127, sample 2
//    channel 0, sample 3
//    channel 1, sample 3
//    ...
//    channel 127, sample 3
//    8 user words
//    parity word
//    trailer word

#include <packet_mutc_dcm2.h>

#include  <string.h>



Packet_mutc_dcm2::Packet_mutc_dcm2(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_mutc_dcm2::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_mutc_dcm2( temp
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

int *Packet_mutc_dcm2::decode_amu (int *nwout)
{
  int *p,*k;

  int dlength = getDataLength();
  if ( dlength < 532 ) 
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

int *Packet_mutc_dcm2::decode_misc (int *nwout)
{
  int *p,*k;
  int i;

  int dlength = getDataLength();
  if ( dlength < 532 ) 
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
  p[2] = k[5] & 0xff;    // Beam Clock Counter


  for (i = 0; i<9; i++)  // 8 user words + parity
    p[3+i] = k[522+i];


  *nwout = 12;
  return p;
}
// ------------------------------------------------------

int  Packet_mutc_dcm2::iValue(const int ich, const char *what)
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

int   Packet_mutc_dcm2::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  if (ich >= 128) return 0;
  if (iy >= 4) return 0;
  return decoded_data1[ich + iy*128];
}

// ------------------------------------------------------

void Packet_mutc_dcm2::dump ( OSTREAM &os) 
{
  int *k;
  int i;

  this->identify(os);

  int dlength = getDataLength();
  if ( dlength < 532 ) return ;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return ;
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
    os << "  Userword " << i << "  " <<   SETW(8) << std::hex <<  k[522+i] << std::dec << std::endl;

  os << "  Long. Parity word   " <<   SETW(8) << std::hex <<  k[530] << std::dec << std::endl;

  int j,l;

  for (j=0; j<128; j++)
    {
      os << std::dec << SETW(5) << j << " |  ";
      for (l=0;l<4;l++) 	os << std::hex << SETW(8) << k[10+j+ 128*l] << " " ;
      os << std::dec << std::endl;
    }
	
}
//---------------------------------------------------------------------------------------------

int   Packet_mutc_dcm2::fillIntArray (int iarr[],
                        const int nlen, int *nwout,
                        const char *what)
{
  int *from;
  int howmuch;
  int dcmword;
  int nwords;

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

  else if (strcmp(what,"DATA") == 0 || strcmp(what,"CHN") == 0 || strcmp(what,"WRDCNT") == 0)
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

  if ( from == 0 ) 
    {
      *nwout = 0;
      return -1;
    }


  nwords = howmuch;

  // and copy the data to the output array

  for (int i=0; i<howmuch; i++) {

    dcmword = *from++;

    if (strcmp(what,"") == 0 || strcmp(what,"RAW") == 0 )
      *iarr++ = dcmword;
    else if (strcmp(what,"DATA") == 0){   // Only pass data words and strip off lower 16 bits
      if (i>9 && i<522) *iarr++ = (dcmword&0x0000FFFF);
      else nwords--;
    }
    else if (strcmp(what,"CHN") == 0) {  // determine what channel number this is
      //      if (i>9 && i<522) *iarr++ = i - 10 - floor((i-10)/128)*128;
      if (i>9 && i<522) *iarr++ = (i-10)%128;
      else nwords--;
    }
    else if (strcmp(what,"WRDCNT") == 0){   // determine what sample number this is
      //      if (i>9 && i<522) *iarr++ = floor((i-10)/128);
      if (i>9 && i<522) *iarr++ = (i-10)/128;
      else nwords--;
    }
  }

  // tell how much we copied
  *nwout = nwords;
  return 0;

}


