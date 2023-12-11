#include <packet_tec_dcm0.h>

Packet_tec_dcm0::Packet_tec_dcm0(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_tec_dcm0::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  // int i;
  int temp[MAX_OUTLENGTH];

  int dlength = getDataLength();

  int status = decode_tec_dcm0( temp
			      ,(int *)  findPacketDataStart(packet) 
			      ,dlength
			      ,MAX_OUTLENGTH, &olength);

  if (status || olength<=0 ) return NULL;
 
  p = new int[olength];
  k = p;
  for (int i =0; i<olength; i++) *k++ = temp[i];
  *nwout = olength;
  return p;
}
// ------------------------------------------------------

int  Packet_tec_dcm0::iValue(const int ich, const int time)
{

  if (ich < 0 || ich > 63 || time < 0 || time > 79) return 0;

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
  

  return decoded_data1[ich*80 + time]; 
 
}

int   Packet_tec_dcm0::fillIntArray (int iarr[],
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
      *nwout = sizeof(struct tecChannelList)*uu;
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

// ------------------------------------------------------

int  Packet_tec_dcm0::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  int *k;
  int dlength = getDataLength();
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }

  if ( strcmp(what,"EVTNR")==0)
    {
      return k[1] & 0xfffff;  // Event number
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      return k[2] & 0xfffff;
    }

  else if ( strcmp(what,"FLAGS")==0)
    {
      return k[3] & 0xfffff;
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      return k[4] & 0xfffff;
    }

  else if ( strcmp(what,"USERWORD")==0)
    {
      return k[dlength-3]; 
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      return k[dlength-2]; 
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



  else	return 0;
}

int Packet_tec_dcm0::decode_to_sparse ( int *p )
{


  int dlength = getDataLength();

  int *k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }

  int i =0;
  int j= 0;
  int count = 0;
  int tcounter = 0;
 
  int *sptr = &k[5];  // here start the data

  int b20;
  int timebase;

  int value;

  while ( (sptr[i] & 0x80000000)  == 0) 
    {

      if ( i >  dlength -5) break;
 	/*
	  test if i is greater than NLEN; we exceed the
	  allowed space in ARR then
	*/
	
	
	b20 = sptr[i] & 0xfffff;  // these are the 20 bits in one 32 bit word.
	
	int upperbits =  (sptr[i] >> 20 ) & 0xfff;
	int wordnr    = upperbits & 0x01f;
	int channel  = (upperbits >> 5 ) & 0x3f;
	
	timebase = wordnr *4;
	
	// get bits 0...4 first
	for (j = 0; j < 4; j++)
	  {
	    value = (b20 >>j*5 ) & 0x1f;
	    if (value) 
	      {
		p[tcounter++] = channel;
		p[tcounter++] = timebase + j;
		p[tcounter++] = value;
		count++;
	      }
	  }


	i++;
	
	
    }
  
  return count;
}

// ------------------------------------------------------

void Packet_tec_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  
  this->identify(os);
  
  
  k = (int *) findPacketDataStart(packet);
  if (k ==0) return;


  os << "  Detector id:        "  << SETW(8) << std::hex << (k[0] &0xfffff) << std::dec << std::endl;    //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[1] &0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[2] &0xfffff) << std::dec << std::endl;    // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[3] &0xfffff) << std::dec << std::endl;    //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[4] &0xfffff) << std::dec << std::endl;    // Beam Clock Counter

  int * packetData = (int *) findPacketDataStart (packet);

  int j = 5;
  int l;
  while (1)
    {
      os << SETW(5) << std::dec << j << " |  ";
      for (l=0;l<4;l++)
	{
	  os << std::hex << SETW(8) << packetData[j++] << " " ;
	  if (j>=getDataLength() ) break;
	}
      if (j>=getDataLength() ) break;
      os << std::endl;
    }
  os << std::endl;

  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;

  dumpErrorBlock(os);
  dumpDebugBlock(os);
  
}
