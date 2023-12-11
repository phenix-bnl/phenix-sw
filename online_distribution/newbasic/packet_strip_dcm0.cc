#include <packet_strip_dcm0.h>

using namespace std;

Packet_strip_dcm0::Packet_strip_dcm0()
{
  memset ( rcc_enabled,0, 6*sizeof(int));
  nr_rcc=0;
  channeldata =0;
  ped_corr=0;
  dib_version=0;
  parity=0;
  parity_2=0;
}

Packet_strip_dcm0::Packet_strip_dcm0(PACKET_ptr data)
  : Packet_w4 (data)
{
  memset ( rcc_enabled,0, 6*sizeof(int));
  nr_rcc=0;
  channeldata =0;
  ped_corr=0;
  dib_version=0;
  parity=0;
  parity_2=0;
}

Packet_strip_dcm0::~Packet_strip_dcm0()
{
}

// ------------------------------------------------------

int  Packet_strip_dcm0::iValue(const int rcc, const int channel)
{
  if ( rcc < 0 || rcc >5) return 0;
  if ( channel < 0 || channel >=12*128) return 0;

  if ( !decoded_data1 )
    {
      decoded_data1 = decode(&data1_length);
    }
  if ( !decoded_data1 ) return 0;
  return decoded_data1[rcc * 12*128 + channel];
}

// ------------------------------------------------------
int Packet_strip_dcm0::iValue(const int ich)
{
  //  cout << __FILE__ << "  " << __LINE__ << " decoded_data1: " << decoded_data1 << endl;
  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }
  //cout << __FILE__ << "  " << __LINE__ << " decoded_data1: " << decoded_data1 << endl;
      
  if (ich >=0 && ich < data1_length) return decoded_data1[ich];

  return 0;
}

// ------------------------------------------------------
int  Packet_strip_dcm0::iValue(const int ich, const char *what)
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

  else if ( strcmp(what,"FLAG")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[1];
    }


  else if ( strcmp(what,"DETID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[2];
    }


  else if ( strcmp(what,"DIBID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[3];
    }


  else if ( strcmp(what,"CIBID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[4];
    }



  else if ( strcmp(what,"BCLCK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[5];
    }

  else if ( strcmp(what,"RCCADDR")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      if ( ich < 0 || ich >5) return 0;
      return decoded_data2[6+ich];
    }

  else if ( strcmp(what,"NR_RCC")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return nr_rcc;
    }

  else if ( strcmp(what,"FIRMWARE")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return dib_version;
    }

  else if ( strcmp(what,"CHANNELDATA")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return channeldata;
    }

  else if ( strcmp(what,"PEDCORRECTED")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      return ped_corr;
    }


  else if ( strcmp(what,"ENABLED")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      if ( ich < 0 || ich >5) return 0;
      return rcc_enabled[ich];
    }


  else if ( strcmp(what,"RCCHYBRID")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      if ( ich < 0 || ich >5) return 0;
      return decoded_data2[12+ich];
    }

  else if ( strcmp(what,"RCCBCLK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      if ( ich < 0 || ich >5) return 0;
      return decoded_data2[18+ich];
    }

//   else if ( strcmp(what,"STATUS")==0)
//     {
//       if (decoded_data2 == NULL )
// 	{
// 	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
// 	    return 0;
// 	}
      
//       return decoded_data2[16];
//     }


   else if ( strcmp(what,"PARITY")==0)
     {
       if (decoded_data2 == NULL )
 	{
 	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
 	    return 0;
 	}
      
       return parity;
     }

   else if ( strcmp(what,"DCMPARITY")==0)
     {
       if (decoded_data2 == NULL )
 	{
 	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
 	    return 0;
 	}
      
       return parity_2;
     }


  return 0;
}



// ------------------------------------------------------

int *Packet_strip_dcm0::decode (int *nwout)
{
  int *p,*k;

  int *p0, *p1;

  int *t;

  int i;
  int dlength = getDataLength();

  p = (int *) findPacketDataStart(packet);
  if (p == 0) 
    {
      *nwout = 0;
      return 0;
    }
  k = &p[17]; // skip the header info

  if ( dlength < 4665 ) // that's the length... 
    {
      *nwout = 0;
      return 0;
    }
  


  int chip, channel;
  
  p0 = new int [ 6 * 128  * 12 ];  // 6*128 ADCs per chip 12 chips
  p1 = new int [ 6 * 12 ];  // 6 numbers per chip, 12 chips
  
#define SVXLENGTH 387
  
  int w, rcc_a, rcc_b;

  w = iValue(0,"NR_RCC"); //just to trigger the "misc" decoding

  for ( chip =0; chip <12; chip ++)
    {
      t = &k[ chip * SVXLENGTH];
      
      // get the "L1 sample RCC" 
      for ( i = 0; i < 3; i++)
	{

	  rcc_a = 2*i;  
	  rcc_b = 2*i+1;  // in this word we are dealing with chip n's rcc numbers a and b

	  w = rcc_a*12 + chip;   // the location where we dump the data
	  if (rcc_enabled[rcc_a])
	    {
	      p1[w] = t[i] & 0xff;
	    }
	  else
	    {
	      p1[w] = 0;
	    }

	  w = rcc_b*12 + chip;
	  if (rcc_enabled[rcc_b])
	    {
	      p1[w] = t[i] = (t[i] >> 8) & 0xff;
	    }
	  else
	    {
	      p1[w] = 0;
	    }
	}
      
      // get the ADCs 
      
      
      for ( channel = 0; channel < 128; channel++)
	{
	  for ( i = 0; i < 3; i++)
	    {
	      rcc_a = 2*i;  
	      rcc_b = 2*i+1;  // in this word we are dealing with chip n's rcc numbers a and b


	      w = rcc_a*12*128 + chip*128 + channel;
	      if (rcc_enabled[rcc_a])
		{
		  p0[w] = t[3 + i + channel *3 ] & 0xff;
		}
	      else
		{
		  p0[w] = 0;
		}


	      w = rcc_b*12*128 + chip*128 + channel;
	      if (rcc_enabled[rcc_b])
                {
		  p0[w] = (t[3 + i + channel *3 ] >> 8) & 0xff;
		}
	      else
		{
                  p0[w] = 0;
                }


	    }
	}
    }
  decoded_data3 = p1;
  data3_length = 12*6;
  
  
  *nwout = 128 * 6 * 12;
  return p0;
}

// ------------------------------------------------------

int *Packet_strip_dcm0::decode_misc (int *nwout)
{

  int *p,*k;
  int i;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  


  if (  k[4662] & 0x8000 )
    {
      channeldata =1;
    }

  if (  k[4662] & 0x4000 )
    {
      ped_corr =1;
    }

  int en = k[4662] & 0x3f; // the enabled bits
  nr_rcc = 0;
  for ( i=0; i <6; i++)
    {
      if ( en &1) 
	{
	  rcc_enabled[5-i] = 1;
	  nr_rcc++;
	}
      en >>=1;
    }

  dib_version = k[4663] & 0xffff;

  parity = k[4664] & 0xffff;
  parity_2 = k[4665] & 0xffff;


  p = new int[33];

  p[0] = k[0] & 0xffff;      // event number
  p[1] = k[1] & 0xffff;      // Flag word 
  p[2] = k[2] & 0xffff;      // Det id       (initially hardwired to 0x1800)
  p[3] = k[3] & 0xff;        // DIB ID
  p[4] = (k[3]>>8) & 0xff;   // CIB ID
  p[5] = k[4] & 0xffff;      // beam clock counter  


  for ( i = 0; i < 3; i++)   // takes p[6]....p[11] and p[12]... p[17]
    {

      if ( rcc_enabled[2*i] )
	{
	  p[6 + 2*i] = k[5+i] & 0x7;  // RCC Address lower of the two 
	  p[12 + 2*i] = k[8+i] & 0xf;  // enabled hybrids lower of the two 
	}
      else
	{
	  p[6 + 2*i] = 0;
	  p[12 + 2*i] = 0;
	}

      if ( rcc_enabled[2*i+1] )
	{
	  p[6 + 2*i+1] = ( k[5+i] >>8)  & 0x7;  // RCC Address higher of the two 
	  p[12 + 2*i+1] = ( k[8+i] >>8)  & 0xf;  // enabled hybrids higher of the two 
	}
      else
	{
	  p[6 + 2*i+1] = 0;
	  p[12 + 2*i+1] = 0;
	}

    }
  
  for ( i = 0; i < 3; i++)   // takes p[18, 19, 20, 21, 22, 23]
    {
      
      if ( rcc_enabled[2*i] )
        {
	  p[18 + 2*i] = ((k[11+i] & 0xff) << 8) + (k[11+i + 3] & 0xff) ;  // Beam Clock Address lower of the two 
	}
      else
	{
	  p[18 + 2*i] =0;
	}

      if ( rcc_enabled[2*i+1] )
        {
	  p[18 + 2*i +1] = ((k[11+i] & 0xff00)) + ((k[11+i + 3] >> 8 )& 0xff) ;  // Beam Clock Address lower of the two 
	}
      else
	{
	  p[18 + 2*i +1] = 0;
	}

    }
  
  for ( i = 24; i < 33; i++)   
    {
      
      p[i] = 0;
    }
  

  *nwout = 33;
  return p;

}

// ------------------------------------------------------

void Packet_strip_dcm0::dump ( OSTREAM &os) 
{

  int rcc;
  int chip;
  int channel;

  this->identify(os);
  
  os << " Event number:                    " << iValue(0,"EVTNR") << std::endl; 
  os << " Det id                           " << std::hex << "0x" << iValue(0,"DETID") << std::dec << std::endl;

  os << " DIB ID                           " << std::hex << "0x" << iValue(0,"DIBID") << std::dec << std::endl;
  os << " DIB Firmware                     " << std::hex << "0x" << iValue(0,"FIRMWARE") << std::dec << std::endl;
  os << " CIB ID                           " << std::hex << "0x" << iValue(0,"CIBID") << std::dec << std::endl;
  os << " module ID                        " << std::hex << "0x" << iValue(0,"MODID") << std::dec << std::endl;
  os << " Flag word                        " << std::hex << "0x" << iValue(0,"FLAG")  << std::dec << std::endl;
  os << " beam clock counter               " << std::hex << "0x" << iValue(0,"BCLCK") << std::dec <<std::endl;
  if ( iValue(0,"CHANNELDATA"))
    {
      os << " Readout mode                     " << "Channel data" <<std::endl;
    }
  else
    {
      os << " Readout mode                     " << "ADC data" <<std::endl;
    }

  if ( iValue(0,"PEDCORRECTED") )
    {
      os << " Pedestal subtracted              " << "Yes" <<std::endl;
    }
  else
    {
      os << " Pedestal subtracted              " << "No" <<std::endl;
    }

  os << " Parity                           " << std::hex << "0x" << iValue(0,"PARITY") << std::dec <<std::endl;
  os << " DCM Parity                       " << std::hex << "0x" << iValue(0,"DCMPARITY") << std::dec <<std::endl;


  os << " RCC enabled                      " << std::hex;
  for ( rcc = 0; rcc < 6; rcc++)
    {
      os << " " << std::setw(8) << iValue(rcc,"ENABLED");
    }
  os<< std::dec << std::endl;

  os << " RCC Addresses                    " << std::hex;
  for ( rcc = 0; rcc < 6; rcc++)
    {
      os << " " << std::setw(8) << iValue(rcc,"RCCADDR");
    }
  os<< std::dec << std::endl;


  os << " RCC Hybrid Enabled               " << std::hex;
  for ( rcc = 0; rcc < 6; rcc++)
    {
      os << " " << std::setw(8) << iValue(rcc,"RCCHYBRID");
    }
  os<< std::dec << std::endl;


  os << " RCC Beam Clock                   " << std::hex;
  for ( rcc = 0; rcc < 6; rcc++)
    {
      os  << " " << std::setw(8) << iValue(rcc,"RCCBCLK");
    }
  os<< std::dec << std::endl;

  //  os << " Error Status                     " << std::hex <<iValue(0,"STATUS") << std::dec << std::endl;
  // os << " Parity                           " << std::hex << iValue(0,"PARITY") << std::dec <<std::endl;

  for ( rcc = 0 ; rcc <6; rcc++)
    {
      if ( iValue(rcc,"ENABLED") )
	{
	  os << " rcc   chn         c0    c1    c2    c3    c4    c5    c6    c7    c8    c9    c10   c11" << endl;
	  os << "----------------------------------------------------------------------------------------" << endl;
	  
	  for ( channel = 0; channel < 128; channel++)
	    {
	      
	      os << setw(3) << rcc << "   " << setw(4) << channel<< "  |  ";
	      
	      
	      for ( chip = 0; chip < 12; chip++)
		{
		  
		  //	      os << std::setw(6) << channel  << " |" ;
		  os << "  " << setw(4) << iValue(rcc, chip*128 + channel);
		  
		}
	      os <<  std::endl;
	    }
	}
    }
  //   dumpErrorBlock(os);
  //  dumpDebugBlock(os);

}








