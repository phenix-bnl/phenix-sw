#include <iostream>
#include "SvxPacket.h"

ClassImp(SvxPacket)



SvxPacket::SvxPacket(){
  for(int i=0; i<13; i++) _packet[i]=0;
  for(int i=13; i<13+4096; i++) _packet[i]=-1;
  for(int i=13+4096; i<SVXPACKETLENGTH; i++) _packet[i]=0;

}

SvxPacket::SvxPacket(const SvxPacket* a) {
  for(int i=0; i<SVXPACKETLENGTH; i++) _packet[i] = a->_packet[i];
}



void SvxPacket::identify(std::ostream& out) const {
  out << "I am a Svx Pixel Packet" << std::endl;
}

//----------------------------------------------------------------------------
int SvxPacket::iValue(const int chip, const int row)
{
  if ( chip < 0 || chip >7) return 0;
  if ( row < 0 || row >255) return 0;

  int *decoded_data1 = NULL;
  int data1_length = 0;

//std::cout << "decoding..." << std::endl;
      decoded_data1 = decode(&data1_length);
//std::cout << "decoding done: " << data1_length << std::endl;

  if ( !decoded_data1 ) return 0;
  int returnvalue = decoded_data1[chip*256 + row];
  delete [] decoded_data1;
  return returnvalue;
}


//----------------------------------------------------------------------------

int SvxPacket::iValue(const int ich, const char *what)
{

  int* decoded_data2 = NULL;
  int data2_length = 0;

  if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      int val = decoded_data2[0];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"DETID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      //return decoded_data2[1];
      int val = decoded_data2[1];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"MODADDR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      //return decoded_data2[2];
      int val = decoded_data2[2];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"TEMPERATURE1")==0)
    {
      if (decoded_data2 == NULL )
        {
          if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
            return 0;
        }

      //return decoded_data2[14];
      int val = decoded_data2[14];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"TEMPERATURE2")==0)
    {
      if (decoded_data2 == NULL )
        {
          if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
            return 0;
        }

      //return decoded_data2[15];
      int val = decoded_data2[15];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"STATUS")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      //return decoded_data2[16];
      int val = decoded_data2[16];
      delete [] decoded_data2;
      return val;
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      //return decoded_data2[17];
      int val = decoded_data2[17];
      delete [] decoded_data2;
      return val;
    }

  return 0;
}

//----------------------------------------------------------------------------

int *SvxPacket::decode (int *nwout)
{
  int *p,*k;
    
  //int dlength = SVXPACKETLENGTH;
//  std::cout << "getting packet pointer..." << std::endl;
  p = (int *) _packet;
  if (p == 0) { *nwout = 0; return 0; }


  k = &p[13];  // skip the header info

  int chip;
  int row;
  
//  std::cout << "new p..." << std::endl;
  p = new int [8*256];  // 8 chips, 256  rows

  //std::cout << "even chips..." << std::endl;
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

  //std::cout << "odd chips..." << std::endl;
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

//----------------------------------------------------------------------------

int *SvxPacket::decode_misc (int *nwout)
{
  int *p,*k;

  k = (int *) _packet;
  if (k == 0) { *nwout = 0; return 0; }
  
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



