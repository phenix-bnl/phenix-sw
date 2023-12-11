#include <packet_pxl_dcm0.h>

Packet_pxl_dcm0::Packet_pxl_dcm0()
{
}

Packet_pxl_dcm0::Packet_pxl_dcm0(PACKET_ptr data)
  : Packet_w4 (data)
{

}

Packet_pxl_dcm0::~Packet_pxl_dcm0()
{
}

// ------------------------------------------------------

int  Packet_pxl_dcm0::iValue(const int chip, const int row)
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

int  Packet_pxl_dcm0::iValue(const int ich, const char *what)
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

  return 0;
}



// ------------------------------------------------------

int *Packet_pxl_dcm0::decode (int *nwout)
{
  int *p,*k;

    
  int dlength = getDataLength();

  p = (int *) findPacketDataStart(packet);
  if (p == 0) 
    {
      *nwout = 0;
      return 0;
    }
  k = &p[13]; // skip the header info

  if ( dlength < 4113 ) // that's the length... 
    {
      *nwout = 0;
      return 0;
    }

  int chip;
  int row;
  
  p = new int [ 8*256];  // 8 chips, 256  rows


  // chips 0,2,4,6  first
  for ( chip =0; chip <8; chip +=2)
    {
      for (row = 0; row < 256; row++)
	{
	  int index_msb = (chip)/2 + row *8; // 0,4,8 for chip0, 1,5,9 for 2
	  int index_lsb =  index_msb + 4;

	  p[chip*256 + row ] = (((k[index_msb] & 0xffff) ^ 0xffff) << 16) 
	    | ((k[index_lsb] & 0xffff) ^ 0xffff);
	}
    }

  // chips 1,3,5,7 then
  for ( chip =1; chip <8; chip +=2)
    {
      for (row = 0; row < 256; row++)
	{

	  int index_msb = 2048 + (chip-1)/2 + row *8; 
	  int index_lsb =  index_msb + 4;

	  p[chip*256 + row ] = (((k[index_msb] & 0xffff) ^ 0xffff) << 16) 
	    | ((k[index_lsb] & 0xffff) ^ 0xffff);

	}
    }
  
  *nwout = 8*256;
  return p;
}

// ------------------------------------------------------

int *Packet_pxl_dcm0::decode_misc (int *nwout)
{
  int *p,*k;


  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  
  p = new int[18];

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

  p[13] = k[4109] & 0xffff;  // FAST OR
  p[14] = k[4110] & 0xffff;  // Ladder temperature
  p[15] = k[4111] & 0xffff;  // Ladder Temperature
  p[16] = k[4112] & 0xffff;  // ERROR STATUS
  p[17] = k[4113] & 0xffff;  // PARITY
 

  *nwout = 18;
  return p;

}

// ------------------------------------------------------

void Packet_pxl_dcm0::dump ( OSTREAM &os) 
{

  int chip;
  int row;
  
  this->identify(os);
  
  os << " Event number:                    " << iValue(0,"EVTNR") << std::endl; 
  os << " Det id                           " << iValue(0,"DETID") << std::endl;

  os << " module address                   " << iValue(0,"MODADDR") << std::endl;
  os << " Flag word                        " << std::hex << iValue(0,"FLAG") <<std::dec << std::endl;
  os << " beam clock counter               " << iValue(0,"BCLCK") << std::endl;

  os << " SPIRO Beam Clock Counter Chip0-3 " << iValue(0,"S_BCLCK03") << std::endl;
  os << " 10-bit event number (0-3)        " << iValue(0,"EVTNR03") << std::endl;
  os << " SPIRO Beam Clock Counter Chip4-7 " << iValue(0,"S_BCLCK47") << std::endl;
  os << " 10-bit event number (4-7)        " << iValue(0,"EVTNR47") << std::endl;
  os << " Negated Beam Clock Counter (0-3) " << iValue(0,"NEGBCLK03") << std::endl;
  os << " Pixel Mask (0-3)                 " << std::hex << iValue(0,"PIXMASK03") << std::dec << std::endl;
  os << " Negated Beam Clock Counter (4-7) " << iValue(0,"NEGBCLK47") << std::endl;
  os << " PixelMask (4-7)                  " << std::hex<< iValue(0,"PIXMASK47") << std::dec << std::endl;

  os << " FAST OR                          " << iValue(0,"FAST_OR") << std::endl;
  os << " Ladder temperature               " << iValue(0,"TEMPERATURE1") << std::endl;
  os << " Ladder Temperature               " << iValue(0,"TEMPERATURE2") << std::endl;
  os << " Error Status                     " << std::hex <<iValue(0,"STATUS") << std::dec << std::endl;
  os << " Parity                           " << std::hex << iValue(0,"PARITY") << std::dec <<std::endl;

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








