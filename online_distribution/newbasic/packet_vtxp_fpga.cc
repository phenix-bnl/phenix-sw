#include <packet_vtxp_fpga.h>

Packet_vtxp_fpga::Packet_vtxp_fpga()
{

  calculated_parity=0;
  parity_comparison=0; 
}

Packet_vtxp_fpga::Packet_vtxp_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
  calculated_parity=0;
  parity_comparison=0; 

}

Packet_vtxp_fpga::~Packet_vtxp_fpga()
{
}

// ------------------------------------------------------

int  Packet_vtxp_fpga::iValue(const int chip, const int row)
{
  if ( chip < 0 || chip >7) return 0;
  if ( row < 0 || row >255) return 0;

  if ( !decoded_data1 )
    {
      decoded_data1 = decode(&data1_length);
    }
  if ( !decoded_data1 ) return 0;
  return decoded_data1[chip*256 + row];
}

int  Packet_vtxp_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now


  if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[0];
    }

  else if ( strcmp(what,"DETID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[1];
    }


  else if ( strcmp(what,"MODADDR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[2];
    }

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[3];
    }

  else if ( strcmp(what,"BCLCK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[4];
    }

  else if ( strcmp(what,"S_BCLCK03")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[5];
    }

  else if ( strcmp(what,"EVTNR03")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[6];
    }

  else if ( strcmp(what,"S_BCLCK47")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[7];
    }

  else if ( strcmp(what,"EVTNR47")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[8];
    }

  else if ( strcmp(what,"NEGBCLK03")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[9];
    }

  else if ( strcmp(what,"PIXMASK03")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[10];
    }

  else if ( strcmp(what,"NEGBCLK47")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[11];
    }

  else if ( strcmp(what,"PIXMASK47")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[12];
    }

  else if ( strcmp(what,"FAST_OR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[13];
    }

  else if ( strcmp(what,"TEMPERATURE1")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[14];
    }

  else if ( strcmp(what,"TEMPERATURE2")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[15];
    }

  else if ( strcmp(what,"STATUS")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[16];
    }


  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[17];
    }

  else if ( strcmp(what,"PARITYOK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[18];
    }

  else if ( strcmp(what,"CHECKPARITY")==0)
    {
      if (! parity_comparison  )
	{
	  calculate_parity();
	}
      return parity_comparison;
    }


  else if ( strcmp(what,"DCMEVENTCOUNT")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[19];
    }

  else if ( strcmp(what,"SPIROEVENTCOUNT")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[20];
    }

  else if ( strcmp(what,"SPIRO_A_SIZEERROR")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[21];
    }

  else if ( strcmp(what,"SPIRO_B_SIZEERROR")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[22];
    }


  else if ( strcmp(what,"SPIRO_A_LOSSLOCK")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[23];
    }

  else if ( strcmp(what,"SPIRO_B_LOSSLOCK")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[24];
    }

  else if ( strcmp(what,"SPIRO_A_BYPASS")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[25];
    }

  else if ( strcmp(what,"SPIRO_B_BYPASS")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[26];
    }

  else if ( strcmp(what,"SPIROPARITYERROR")==0)
   {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return decoded_data2[27];
    }



  return 0;
}



// ------------------------------------------------------

int *Packet_vtxp_fpga::decode (int *nwout)
{
  int *p,*k;

  p = (int *) findPacketDataStart(packet);
  if (p == 0) 
    {
      *nwout = 0;
      return 0;
    }
  k = &p[13]; // skip the header info

  int chip;
  int row;
  
  p = new int [ 8*256];  // 8 chips, 256  rows

  memset (p, 0, 8*256*sizeof(int));

  int i = 0;
  while ( ( k[i] & 0x20000000) ==0 )  // bit 29 
    {

      int index = (k[i] >> 16) & 0x1fff;

      if ( index < 2048)   // chips 0,2,4,6  first
	{
	  chip = (index)%8;
	  row  = (index - chip) /8;
	  if ( chip < 4)  // we have the lsb
	    {
	      chip *=2; // make 0,1,2,3 to 0, 2, 4, 6
	      p[chip*256 + row ] |= (( (k[i] & 0xffff) ^ 0xffff  ) << 16);  
	    }
	  else
	    {
	      chip -= 4 ; // now we get the MSBs...
	      chip *=2;   // make 0,1,2,3 to 0, 2, 4, 6
	      p[chip*256 + row ] |= ( (k[i] & 0xffff) ^ 0xffff  );  
	    }
	}
      else  //if index is >=  2048 we have chips 1,3,5,7
 	{
	  chip = (index)%8;
	  row  = (index - 2048 - chip) /8;
	  if ( chip < 4)    // we have the lsb
	    {
	      chip = chip*2 + 1; // make 0,1,2,3 to 1, 3, 5, 7
	      p[chip*256 + row ] |= (( (k[i] & 0xffff) ^ 0xffff  ) << 16);  
	    }
	  else
	    {
	      chip -= 4 ;     // now we get the MSBs...
	      chip = chip*2 + 1;   // make 0,1,2,3 to 1, 3, 5, 7
	      p[chip*256 + row ] |= ( (k[i] & 0xffff) ^ 0xffff  );  
	    }
	}
      i++;
    }

  
  *nwout = 8*256;
  return p;
}

// ------------------------------------------------------

int Packet_vtxp_fpga::calculate_parity ()
{
  int *k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }

  int dlength = getDataLength();

  int i;
  int p = 0;

  for (i=0; i< dlength-2; i++)
    {
      p = p ^ (k[i] & 0xffff);
    }

  if (  (dlength &1) == 0 )  // even number of words
    {
      p = p ^ 0xffff;
    }

  calculated_parity = p;

  if( p == iValue(0, "PARITY") )
    {
      parity_comparison = 1;
    }
  else
    {
      parity_comparison = -1;
    }

  return 0;
}



// ------------------------------------------------------

int *Packet_vtxp_fpga::decode_misc (int *nwout)
{
  int *p,*k;


  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }

  int dlength = getDataLength();
  
  p = new int[28];

  p[0] = k[0] & 0xffff;  // event number
  p[1] = k[1] & 0xffff;  // Det id
  p[2] = k[2] & 0xffff;  // module address
  p[3] = k[3] & 0xffff;  // Flag word
  p[4] = k[4] & 0xffff;  // beam clock counter

  p[5] = k[5] & 0xffff;  // SPIRO Beam Clock Counter Chip0-3
  p[6] = k[6] & 0xffff;  // 10-bit event number (0-3)
  p[7] = k[7] & 0xffff;  // SPIRO Beam Clock Counter Chip4-7
  p[8] = k[8] & 0xffff;  // 10-bit event number (4-7)
  p[9] = k[9] & 0xffff;  // Negated Beam Clock Counter (0-3)
  p[10] = k[10] & 0xffff;  // Pixel Mask (0-3)
  p[11] = k[11] & 0xffff;  // Negated Beam Clock Counter (4-7)
  p[12] = k[12] & 0xffff;  // PixelMask (4-7)

  p[13] = k[dlength-6] & 0xffff;  // FAST OR
  p[14] = k[dlength-5] & 0xffff;  // Ladder temperature
  p[15] = k[dlength-4] & 0xffff;  // Ladder Temperature
  p[16] = k[dlength-3] & 0xffff;  // ERROR STATUS
  p[17] = k[dlength-2] & 0xffff;  // PARITY
  p[18] = (k[dlength-1] >>8) & 0x1;  // PARITY CHECK
  p[19] = (k[dlength-1] >>4) & 0xf;  // Chi Event Count
  p[20] = p[16] & 0xf;               // Spiro event count
  p[21] = (p[16]>>4) & 0x1;           // Spiro channel A size error
  p[22] = (p[16]>>5) & 0x1;           // Spiro channel B size error
  p[23] = (p[16]>>11) & 0x1;          // Spiro channel A Loss of Lock
  p[24] = (p[16]>>12) & 0x1;          // Spiro channel B Loss of Lock
  p[25] = (p[16]>>13) & 0x1;          // Spiro channel A Bypassed
  p[26] = (p[16]>>14) & 0x1;          // Spiro channel B Bypassed
  p[27] = (p[16]>>15) & 0x1;          // Spiro Parity Error


  *nwout = 28;
  return p;

}

// ------------------------------------------------------

void Packet_vtxp_fpga::dump ( OSTREAM &os) 
{

  int chip;
  int row;
  
  this->identify(os);
  
  os << " Event number:                    " << iValue(0,"EVTNR") << std::endl; 
  os << " Det id                           " << iValue(0,"DETID") << std::endl;

  os << " module address                   " << iValue(0,"MODADDR") << std::endl;
  os << " Flag word                      0x" << std::hex << iValue(0,"FLAG") <<std::dec << std::endl;
  os << " beam clock counter               " << iValue(0,"BCLCK") << std::endl;

  os << " SPIRO Beam Clock Counter Chip0-3 " << iValue(0,"S_BCLCK03") << std::endl;
  os << " 10-bit event number (0-3)        " << iValue(0,"EVTNR03") << std::endl;
  os << " SPIRO Beam Clock Counter Chip4-7 " << iValue(0,"S_BCLCK47") << std::endl;
  os << " 10-bit event number (4-7)        " << iValue(0,"EVTNR47") << std::endl;
  os << " Negated Beam Clock Counter (0-3) " << iValue(0,"NEGBCLK03") << std::endl;
  os << " Pixel Mask (0-3)               0x" << std::hex << iValue(0,"PIXMASK03") << std::dec << std::endl;
  os << " Negated Beam Clock Counter (4-7) " << iValue(0,"NEGBCLK47") << std::endl;
  os << " PixelMask (4-7)                0x" << std::hex<< iValue(0,"PIXMASK47") << std::dec << std::endl;

  os << " FAST OR                          " << iValue(0,"FAST_OR") << std::endl;
  os << " Ladder temperature               " << iValue(0,"TEMPERATURE1") << std::endl;
  os << " Ladder Temperature               " << iValue(0,"TEMPERATURE2") << std::endl;
  os << " Error Status                   0x" << std::hex <<iValue(0,"STATUS") << std::dec << std::endl;
  os << " DCM Event Count                  "  <<iValue(0,"DCMEVENTCOUNT") << std::endl;
  os << " Spiro Event Count                "  <<iValue(0,"SPIROEVENTCOUNT") << std::endl;
  os << " Spiro A Size error               "  <<iValue(0,"SPIRO_A_SIZEERROR") << std::endl;
  os << " Spiro B Size error               "  <<iValue(0,"SPIRO_B_SIZEERROR") << std::endl;
  os << " Spiro A Loss of Lock             "  <<iValue(0,"SPIRO_A_LOSSLOCK") << std::endl;
  os << " Spiro B Loss of Lock             "  <<iValue(0,"SPIRO_B_LOSSLOCK") << std::endl;
  os << " Spiro A Bypass                   "  <<iValue(0,"SPIRO_A_BYPASS") << std::endl;
  os << " Spiro B Bypass                   "  <<iValue(0,"SPIRO_B_BYPASS") << std::endl;
  os << " Spiro Parity Error               "  <<iValue(0,"SPIROPARITYERROR") << std::endl;
  os << " Parity                           " << std::hex << iValue(0,"PARITY") << std::dec <<std::endl;
  os << " Parity Ok                        ";
  if ( iValue(0,"PARITYOK") ==1 )
    {
      os << "Yes" <<std::endl;
    }
  else
    {
      os << "No    (0x" << std::hex << calculated_parity << ")" << std::dec << std::endl;
    }

  os << " Parity Check                     " << std::hex << iValue(0,"CHECKPARITY") << std::dec <<std::endl;

  for ( row = 0; row < 256; row++)
    {
      os << std::setw(4) << row << " |" << std::hex;
      for ( chip = 0; chip < 8; chip++)
	{
	  os << "  " << std::setw(6) << iValue(chip,row);
	}
      os << std::dec << std::endl;
    }
  
  //   dumpErrorBlock(os);
  //  dumpDebugBlock(os);

}








