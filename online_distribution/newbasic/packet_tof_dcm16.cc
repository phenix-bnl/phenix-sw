#include <packet_tof_dcm16.h>



// ------------------------------------------------------

int  Packet_tof_dcm16::iValue(const int ich, const char *what)
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

  else if (strcmp(what,"TC1") == 0)  // user requested T C1 info
    {			

      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}

      if (ich > data1_length || ich < 0) return 0;

      return decoded_data2[ich];
    }


  else if (strcmp(what,"QC2") == 0)  // user requested Q C2 info
    {			

      if (decoded_data1 == NULL )
	{
	  if ( (decoded_data1 = decode(&data1_length))==NULL)
	    return 0;
	}

      if (ich > data3_length || ich < 0) return 0;

      return decoded_data3[ich];
    }

  else if (strcmp(what,"TC2") == 0)  // user requested T C2 info
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

      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[ich*3];
    }


  else if ( strcmp(what,"AMU1")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[ich*3+1];
    }


  else if ( strcmp(what,"AMU2")==0)
    {

      if (ich > 15 || ich < 0) return 0;

      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[ich*3+2];
    }




  else if ( strcmp(what,"SMARKER")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[50];
    }


  else if ( strcmp(what,"DETID")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[51];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[52];
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}

      return decoded_data5[53];
    }

  else if ( strcmp(what,"FLAGWORD")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}

      return decoded_data5[54];
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      
      return decoded_data5[55];
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data5 == NULL )
	{
	  if ( (decoded_data5 = decode_misc(&data5_length))==NULL)
	    return 0;
	}
      return decoded_data5[56];
    }

  else	return 0;
}




Packet_tof_dcm16::Packet_tof_dcm16(PACKET_ptr data)
  : Packet_w4 (data){}
  

int *Packet_tof_dcm16::decode ( int *nwout)
{
  int *k;



  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  tof_dcm16_board *b =  (tof_dcm16_board *) &k[6];

  int board;
  int l,c;
  int current = 0;

  int *q1 = new int[16*16];
  int *t1 = new int[16*16];
  int *q2 = new int[16*16];
  int *t2 = new int[16*16];

  for (board = 0; board < 16; board++)  // we loop over the 16 boards
    {

      // "data1" will be the "Q-C1" data
      // "data2" will be the "T-C1" data
      // "data3" will be the "Q-C2" data
      // "data4" will be the "T-C2" data
      for (l = 0; l <4; l++)   // we do crossing 1 (c1) first
	{
	  for (c = 0; c <4; c++)
	    {
	      q1[current] = (b->c1.channeldata[c*8 + l] & 0xfff) ^ 0x800;
	      t1[current] = (b->c1.channeldata[c*8 +4 + l] & 0xfff) ^ 0x800;
	      q2[current] = (b->c2.channeldata[c*8 + l] & 0xfff) ^ 0x800;
	      t2[current] = (b->c2.channeldata[c*8 +4 + l] & 0xfff) ^ 0x800;
	      current++;
	    }
	}


      b++;  // move to the next board
    }


  decoded_data2 = t1;
  data2_length  = 16*16;

  decoded_data3 = q2;
  data3_length  = 16*16;

  decoded_data4 = t2;
  data4_length  = 16*16;




  *nwout = 16*16;
  return q1;
}

int *Packet_tof_dcm16::decode_misc ( int *nwout)
{
  int *k;
  int dlength = getDataLength();


  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  tof_dcm16_board *b =  (tof_dcm16_board *) &k[6];

  int board;
  int l;
  int current = 0;

  int *misc = new int[57];  

  for (board = 0; board < 16; board++)  // we loop over the 16 boards
    {

      misc[current++] = (b->FEMaddr    & 0xfff);
      misc[current++] = (b->c1.AMUcell & 0x3f);
      misc[current++] = (b->c2.AMUcell & 0x3f);
 
      b++;  // move to the next board
    }

  current = 50;

  for (l = 0; l<6; l++) // the FEM header
    {
      misc[current++] = k[l] & 0xffff; 
    }


  misc[56] = (k[dlength-2] & 0xffff);  // the parity word

  *nwout = 57;
  return misc;
}



// ------------------------------------------------------

void Packet_tof_dcm16::dump ( OSTREAM &os) 
{
  int *k;
  
  this->identify(os);
  
  int dlength = getDataLength();
  //  if ( dlength <  payloadlength ) return ;
  
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }
  
  os << "  Start marker word : "  << SETW(8) << std::hex <<  k[0] << std::dec << std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex <<  k[1] << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[2] & 0xffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[3] & 0xffff) << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[4] & 0xffff) << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[5] & 0xffff) << std::dec << std::endl;    // Beam Clock Counter
  os << "  long. Parity:       "  << SETW(8) << std::hex << (k[dlength-2] & 0xffff) << std::dec << std::endl; 
  

  tof_dcm16_board *b =  (tof_dcm16_board *) &k[6];

  int board;
  int l,c;
  int channel = 0;

  for (board = 0; board < 16; board++)  // we loop over the 16 boards
    {

      os << "board number " << SETW(4) << board 
	   << " crossing 1 AMU: " << ( b->c1.AMUcell & 0x1f)
	   << " crossing 2 AMU: " << ( b->c2.AMUcell & 0x1f)
	   << std::endl;

      os << "          Q(C1) Q(C2) T(C1) T(C2)    Q(C1) Q(C2) T(C1) T(C2)" << std::endl;

      for (l = 0; l <4; l++)   // we do crossing 1 (c1) first
	{
	  for (c = 0; c <4; c++)
	    {
	      os << SETW(5) << channel << " | " << std::hex;

	      os << SETW(6) <<  ( b->c1.channeldata[c*8 + l] & 0xfff);
	      os << SETW(6) <<  ( b->c2.channeldata[c*8 + l] & 0xfff);

	      os << SETW(6) <<  ( b->c1.channeldata[c*8 +4 + l] & 0xfff);
	      os << SETW(6) <<  ( b->c2.channeldata[c*8 +4 + l] & 0xfff);

	      os << "   ";
	      os << SETW(6) <<  (( b->c1.channeldata[c*8 + l] & 0xfff) ^ 0x800);
	      os << SETW(6) <<  (( b->c2.channeldata[c*8 + l] & 0xfff) ^ 0x800);

	      os << SETW(6) <<  (( b->c1.channeldata[c*8 +4 + l] & 0xfff) ^ 0x800);
	      os << SETW(6) <<  (( b->c2.channeldata[c*8 +4 + l] & 0xfff) ^ 0x800);

	      os << std::dec << std::endl;
	      channel++;
	    }
	}
      b++;  // move to the next board
    }


   dumpErrorBlock(os);
   dumpDebugBlock(os);

}
