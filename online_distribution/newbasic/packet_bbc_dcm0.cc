#include <packet_bbc_dcm0.h>

Packet_bbc_dcm0::Packet_bbc_dcm0()
{
}

Packet_bbc_dcm0::Packet_bbc_dcm0(PACKET_ptr data)
  : Packet_w4 (data)
{
  payloadlength = 408;
  no_boards = 16;
}

Packet_bbc_dcm0::~Packet_bbc_dcm0()
{
}

// ------------------------------------------------------

int  Packet_bbc_dcm0::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if (strcmp(what,"T1") == 0)  // user requested T1 info
    {			
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_t1(&data2_length))==NULL)
	    return 0;
	}
      if (ich > data2_length) return 0;

      return decoded_data2[ich];
    }

  else if (strcmp(what,"T2") == 0)  // user requested T1 info
    {			
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_t2(&data3_length))==NULL)
	    return 0;
	}
      if (ich > data3_length) return 0;

      return decoded_data3[ich];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      return decoded_data4[0];
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[1];
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      
      return decoded_data4[2];
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      return decoded_data4[3];
    }

  else if ( strcmp(what,"BOARD")==0)
    {

      if (ich < 0 || ich >= no_boards) return 0;
      int *k = (int *) findPacketDataStart(packet);
      if (k == 0) 
	{
	  return 0;
	}

      return k[6+ 25*ich];
    }

  else	return 0;
}


int  Packet_bbc_dcm0::iValue(const int ich, const int board)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
  if (ich < 0 || ich > 7) return 0;
  if (board < 0 || board > 15) return 0;
  
  return decoded_data1[ich + board*8];
}


// ------------------------------------------------------

int *Packet_bbc_dcm0::decode (int *nwout)
{
  int *p,*k;

    
  int dlength = getDataLength();
  if ( dlength < payloadlength ) 
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
  p = new int[128];
  int current = 0;
  
  int board;
  int channel;
  for (board = 0; board < no_boards; board++)
    {
      for (channel = 0; channel < 8; channel++)
	{
	  p[current++] = (k[6 + 25*board + 1  + channel*3] & 0xffff);
	}
    }
  
  *nwout = 128;
  return p;
}


// ------------------------------------------------------

int *Packet_bbc_dcm0::decode_t1 (int *nwout)
{
  int *p,*k;
  
  int dlength = getDataLength();
  if ( dlength <  payloadlength ) 
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
  p = new int[128];
  int current = 0;
  
  int board;
  int channel;
  for (board = 0; board < no_boards; board++)
    {
      for (channel = 0; channel < 8; channel++)
	{
	  p[current++] = (k[6 + 25*board+1  + channel*3 +1] & 0xffff) ;
	}
    }
  
  *nwout = 128;
  return p;
}

// ------------------------------------------------------

int *Packet_bbc_dcm0::decode_t2 (int *nwout)
{
  int *p,*k;
  
  int dlength = getDataLength();
  if ( dlength <  payloadlength ) 
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
  p = new int[128];
  int current = 0;
  
  int board;
  int channel;
  for (board = 0; board < no_boards; board++)
    {
      for (channel = 0; channel < 8; channel++)
	{
	  p[current++] = (k[6+ 25*board+1  + channel*3 +2] & 0xffff);
	}
    }
  
  *nwout = 128;
  return p;
}





// ------------------------------------------------------

int *Packet_bbc_dcm0::decode_misc (int *nwout)
{
  int *p,*k;

  int dlength = getDataLength();
  if ( dlength <  payloadlength) 
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

  p[0] = k[2] & 0xffff;  // Event number
  p[1] = k[3];           // Module Address
  p[2] = k[5] & 0xffff;  // Beam Clock Counter
  p[3] = k[payloadlength];         // parity
  

  *nwout = 4;
  return p;
}
// ------------------------------------------------------

void Packet_bbc_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  
  this->identify(os);
  
  int dlength = getDataLength();
  
  COUT << "packet length = " << getPacketLength(packet)  <<std::endl;
  COUT << "data length = " << dlength  <<std::endl;
  COUT << "error length = " <<  getPacketErrorLength(packet)  <<std::endl;
  COUT << "debug length = " << getPacketDebugLength(packet)  <<std::endl;
//  if ( dlength <  payloadlength ) return ;
  
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }
  
  os << "  Start marker word : "  << SETW(8) << std::hex <<  k[0] << std::dec <<std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[1] << std::dec <<std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[2] & 0xffff) << std::dec <<std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[3] & 0xffff) << std::dec <<std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[4] & 0xffff) << std::dec <<std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[5] & 0xffff) << std::dec <<std::endl;    // Beam Clock Counter
  
  
  int board;
  int channel;
  for (board = 0; board < no_boards; board++)
    {
      os << "Board Number: " << k[6+ 25*board] <<std::endl;
      for (channel = 0; channel < 8; channel++)
	{
	  os << SETW(8) << std::hex << (k[6+ 25*board+1  + channel*3   ] & 0xfff);
	  os << SETW(8) << (k[6+ 25*board+1  + channel*3 +1] & 0xfff);
	  os << SETW(8) << (k[6+ 25*board+1  + channel*3 +2] & 0xfff) << std::dec <<std::endl;
	}
    }

   dumpErrorBlock(os);
   dumpDebugBlock(os);

}








