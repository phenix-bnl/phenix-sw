#include <packet_tof_fpga.h>

// ------------------------------------------------------

int  Packet_tof_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if (strcmp(what,"QC1") == 0)  // user requested Q C1 info
    {

      if (decoded_data1 == NULL )
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }

      if (ich > data1_length || ich < 0) return 0;

      return decoded_data1[ich];
    }

  else if (strcmp(what,"QC3") == 0)  // user requested Q C2 info
    {

      if (decoded_data1 == NULL )
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }

      if (ich > data2_length || ich < 0) return 0;

      return decoded_data2[ich];
    }


  else if (strcmp(what,"TC3") == 0)  // user requested T C3 info
    {

      if (decoded_data1 == NULL )
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }
      if (ich > data3_length || ich < 0) return 0;

      return decoded_data3[ich];
    }

  else if (strcmp(what,"TC4") == 0)  // user requested T C4 info
    {

      if (decoded_data1 == NULL )
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }

      if (ich > data4_length || ich < 0) return 0;

      return decoded_data4[ich];
    }


  else if ( strcmp(what,"FEM")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data7 == NULL )
        {
          if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
            return 0;
        }
      return decoded_data6[ich*3 +0];
    }

  else if ( strcmp(what,"FEMEVTCTR")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data7 == NULL )
        {
          if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
            return 0;
        }
      return decoded_data6[ich*3 +1];
    }

  else if ( strcmp(what,"FEMSTATUS")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data7 == NULL )
        {
          if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
            return 0;
        }
      return decoded_data6[ich*3 + 2];
    }


  else if ( strcmp(what,"AMU1")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data1 == NULL )  // no mistake - decode decodes this on the side
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }
      return decoded_data5[ich*3+0];
    }

  //  else if ( strcmp(what,"AMU2")==0)
  //    {
  //
  //      if (ich > 15 || ich < 0) return 0;
  //
  //      if (decoded_data1 == NULL )  // no mistake - decode decoes this 
  //        {
  //          if ( (decoded_data1 = decode(&data1_length))==NULL)
  //            return 0;
  //        }
  //      return decoded_data5[ich*4+1];
  //    }

  else if ( strcmp(what,"AMU3")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data1 == NULL )  // no mistake - decode decoes this 
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }
      return decoded_data5[ich*3+1];
    }

  else if ( strcmp(what,"AMU4")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data1 == NULL )  // no mistake - decode decoes this 
        {
          if ( (decoded_data1 = decode(&data1_length))==NULL)
            return 0;
        }
      return decoded_data5[ich*3+2];
    }



  else if ( strcmp(what,"ID")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}
      return decoded_data7[0];  // detector id in p array is word 0
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}
      return decoded_data7[1];  // word 1 in p array in event number
    }


  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}

      return decoded_data7[2];   // module address stored in p as word 2
    }

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}

      return decoded_data7[3];  // flag word stored in p as word 3
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}

      return decoded_data7[4];  // beam clock counter stored in p as word 4
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}

      return decoded_data7[5];  // longitudinal parity word stored in p as word 5
    }

  else if ( strcmp(what,"SUMMARY")==0)
    {
      if (decoded_data7 == NULL )
	{
	  if ( (decoded_data7 = decode_misc(&data7_length))==NULL)
	    return 0;
	}

      return decoded_data7[6];  // dcm summary word stored in p as word 6
    }

  else if ( strcmp(what,"CHECKPARITY")==0)
    {

      if (getHitFormat() == IDTOF_FPGA0SUP ) return 0; // can't do it

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



  else  return 0;
}

Packet_tof_fpga::Packet_tof_fpga(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_tof_fpga::decode ( int *nwout)
{
  int *p,*k;

  int dlength = getDataLength();

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }



  p = new int[256];
  decoded_data2 = new int[256];
  decoded_data3 = new int[256];
  decoded_data4 = new int[256];

  decoded_data5 = new int[3*16]; // holds AMU info

  memset (p, 0, 256*sizeof(int) );
  memset (decoded_data2, 0, 256*sizeof(int) );
  memset (decoded_data3, 0, 256*sizeof(int) );
  memset (decoded_data4, 0, 256*sizeof(int) );

  memset (decoded_data5, 0, 3*16*sizeof(int) );

  data2_length = 256;
  data3_length = 256;
  data4_length = 256;

  data5_length = 3*16;



  int pos=5;
  int ch;
  int b;

  // find all channel data
  while ( (k[pos]  & 0xf0000000) ==0 )
    {
      if (pos >= dlength) break;

      ch = (k[pos] >> 20 ) & 0xff;

      if ( ch > 255 || ch < 0 ) 
	{
	  std::cout << __FILE__ << "  " << __LINE__ << " : channel out ouf bounds " << ch << std::endl;
	}
      else
	{
	  switch ( ( (k[pos] >> 16 ) & 0xf))
	    {
	    case 0xc:  //T4
	      decoded_data4[ch] = k[pos] & 0xfff;
	      break;
	    case 0xb:  //T3
	      decoded_data3[ch] = k[pos] & 0xfff;
	      break;
	    case 0xa:  //Q3
	      decoded_data2[ch] = k[pos] & 0xfff;
	      break;
	    case 0x9:  //Q1
	      p[ch] = k[pos] & 0xfff;
	      break;

	      // for the AMU, ch holds the channel number. The 
	      // upper 4 bits (or, ch /4) holds the board number.
	    case 0x4:  //T4 AMU
	      b = (ch >> 4);
	      decoded_data5[b*3+2] = k[pos] & 0x3f; 
	      break;
	    case 0x3:  //T3 AMU
	      b = (ch >> 4);
	      decoded_data5[b*3+1] = k[pos] & 0x3f; 
	      break;
	      //	case 0x2:  //Q2 AMU
	      //	  b = (ch >> 4) -1;
	      //	  decoded_data5[b*4+1] = k[pos] & 0x3f; 
	      break;
	    case 0x1:  //Q1 AMU
	      b = (ch >> 4);
	      decoded_data5[b*3+0] = k[pos] & 0x3f; 
	      break;

	    default:
	      break;
	    }
	}
      pos++;
    }
  *nwout = 256;
  return p;
}

int *Packet_tof_fpga::decode_misc ( int *nwout)
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

  decoded_data6 = new int[3*16]; // holds FEM info
  memset (decoded_data6, 0, 3*16*sizeof(int) );
  data6_length = 3*16;

  for (i=dlength-3*16-2-2; i< dlength-2; i++)
    {
      if( ( k[i] & 0x10000000)== 0x10000000)
	{
	  // found the userword start
	  break;
	}
    }

  int j;
  if ( i < 0 || i>dlength) 
    {
      for (j=0; j<3*16; j++)
	{
	  decoded_data6[j] = 0;
	}
      *nwout = 7;
      return p;
    }
  else
    {
      for (j=0; j<3*16; j++)   // we get 16 ID's
	{
	  if ( i+j < dlength &&  i+j >0 )
	    {
	      decoded_data6[j] = k[i+j] & 0xffff;
	    }
	  else
	    {
	      decoded_data6[j] = 0;
	    }
	}

      *nwout = 7;
      return p;
    }
}

// ------------------------------------------------------
void Packet_tof_fpga::dump ( OSTREAM &os) 
{
  this->identify(os);

  int board, c;

  for (board = 0; board < 16 ; board++)  // we loop over the boards
    {

      os << "board number       " << SETW(4) << board << std::endl;
      os << "FEM Id:            " << SETW(8) << std::hex << iValue(board,"FEM") << std::endl
	 << "FEM event counter: " << SETW(8) << std::hex << iValue(board,"FEMEVTCTR")  << std::endl
	 << SETW(8) << std::hex << iValue(board,"FEMSTATUS") << std::dec << std::endl;
 

      os << std::hex << " Q1 AMU: " << iValue(board,"AMU1") << std::endl
	 << " T3 AMU: " << iValue(board,"AMU3") << std::endl
	 << " T4 AMU: " << iValue(board,"AMU4") << std::dec << std::endl;

      os << "          Q(C1) Q(C3) T(C3) T(C4)" << std::endl;

      for (c = 0; c <16; c++)
	{
	  os << SETW(5) << c << " | " << std::hex;
	  os << SETW(6) << iValue(board*16 +c, "QC1");
	  os << SETW(6) << iValue(board*16 +c, "QC3");
	  os << SETW(6) << iValue(board*16 +c, "TC3");
	  os << SETW(6) << iValue(board*16 +c, "TC4");
	  os << std::dec << std::endl;
	}
    }

  os << " --- Parity Check: " << iValue(0,"CHECKPARITY") << std::endl;
  
  dumpErrorBlock(os);
  dumpDebugBlock(os);

  os << std::endl;
}


