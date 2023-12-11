#include <packet_bbc_fpga.h>

Packet_bbc_fpga::Packet_bbc_fpga()
{
}

Packet_bbc_fpga::Packet_bbc_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
  no_boards = 16;
}

Packet_bbc_fpga::~Packet_bbc_fpga()
{
}


// ------------------------------------------------------
int Packet_bbc_fpga::iValue(const int ich)
{
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
      
  if (ich >=0 && ich < data1_length) return decoded_data1[ich];

  return 0;
}


int  Packet_bbc_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if (strcmp(what,"T1") == 0)  // user requested T1 info
    {			
      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}
      // no mistake - decode decodes all three arrays.
      if (ich < 0 || ich >= data2_length) return 0;

      return decoded_data2[ich];
    }

  else if (strcmp(what,"T2") == 0)  // user requested T1 info
    {			
      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}
      if (ich < 0 || ich >= data3_length) return 0;

      return decoded_data3[ich];
    }

  else if ( strcmp(what,"ID")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      return decoded_data4[0];  // detector id in p array is word 0
    }

  else if ( strcmp(what,"BOARD")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      if (ich < 0 || ich >= data5_length) return 0;
      
      return decoded_data5[ich];  // detector id in p array is word 0
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}
      return decoded_data4[1];  // word 1 in p array in event number
    }


  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[2];   // module address stored in p as word 2
    }

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[3];  // flag word stored in p as word 3
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[4];  // beam clock counter stored in p as word 4
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[5];  // longitudinal parity word stored in p as word 5
    }

  else if ( strcmp(what,"SUMMARY")==0)
    {
      if (decoded_data4 == NULL )
	{
	  if ( (decoded_data4 = decode_misc(&data4_length))==NULL)
	    return 0;
	}

      return decoded_data4[6];  // dcm summary word stored in p as word 6
    }


 else if ( strcmp(what,"CHECKPARITY")==0)
    {

      if (getHitFormat() == IDBBC_FPGA0SUP ) return 0; // can't do it

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


int  Packet_bbc_fpga::iValue(const int ich, const int board)
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

int *Packet_bbc_fpga::decode (int *nwout)
{
  int *p,*k;
  
  int dlength = getDataLength();

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  p = new int[128];
  decoded_data2 = new int[128];
  decoded_data3 = new int[128];

  memset (p, 0, 128*sizeof(int) );
  memset (decoded_data2, 0, 128*sizeof(int) );
  memset (decoded_data3, 0, 128*sizeof(int) );
  data2_length = 128;
  data3_length = 128;

  int pos=5;
  int ch;

  // find all channel data
  while ( (k[pos]  & 0xf0000000) ==0 )
    {
      if (pos >= dlength) break;

      ch = (k[pos] >> 20 ) & 0x7f;
      
      switch ( ( (k[pos] >> 16 ) & 0xf))
	{
	case 0xc:
	  decoded_data2[ch] = k[pos] & 0xffff;
	  break;
	case 0xb:
	  decoded_data3[ch] = k[pos] & 0xffff;
	  break;
	case 0xa:
	  p[ch] = k[pos] & 0xffff;
	  break;
	default:
	  break;
	}
      pos++;
    }
  *nwout = 128;
  return p;
}


// ------------------------------------------------------

int *Packet_bbc_fpga::decode_misc (int *nwout)
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

  for (i=dlength-19; i< dlength-2; i++)
    {
      if( ( k[i] & 0x10000000)== 0x10000000)
	{
	  // found the userword start
	  break;
	}
    }


  decoded_data5 = new int[16]; // to hold the board ID's
  data5_length = 16;
  int j;
  for (j=0; j<16; j++)   // we get 16 ID's
    {
      decoded_data5[j] = k[i+j] & 0xffff;
    }

  *nwout = 7;
  return p;
}


void Packet_bbc_fpga::dump ( OSTREAM &os) 
{
  this->identify(os);
  
  os << "  Detector id:        "  << SETW(8) << std::hex << iValue(0,"ID") << std::dec << std::endl;     // Detector ID
  os << "  Event number:       "  << SETW(8) << std::hex << iValue(0,"EVTNR") << std::dec << std::endl;  // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << iValue(0,"MODULE") << std::dec << std::endl; // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << iValue(0,"FLAG") << std::dec << std::endl;   // Flag Word
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << iValue(0,"BCLK") << std::dec << std::endl;   // Beam Clock 

  os << "  Long. Parity word   " <<   SETW(8) << std::hex << iValue(0,"PARITY") << std::dec << std::endl;
  os << "  Sumary word         " <<   SETW(8) << std::hex << iValue(0,"SUMMARY") << std::dec << std::endl;

  os << "     Q       T1      T2" << std::endl;
  os << "--------------------------" << std::endl;     
  int board;
  int channel;
  for (board = 0; board < no_boards; board++)
    {
      os << "Board Number: " << iValue(board,"BOARD")  << std::endl;
      for (channel = 0; channel < 8; channel++)
	{
	  os << SETW(8) << iValue(channel + 8*board);
	  os << SETW(8) << iValue(channel + 8*board,"T1");
	  os << SETW(8) << iValue(channel + 8*board,"T2") << std::dec << std::endl;
	}
    }
  
  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;

   dumpErrorBlock(os);
   dumpDebugBlock(os);

}








