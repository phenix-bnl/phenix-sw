#include <packet_mutrig_fpga.h>

using namespace std;

Packet_mutrig_fpga::Packet_mutrig_fpga()
{

  parity=0;
}

Packet_mutrig_fpga::Packet_mutrig_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
  parity=0;
}

Packet_mutrig_fpga::~Packet_mutrig_fpga()
{
}

// ------------------------------------------------------

int  Packet_mutrig_fpga::iValue(const int rcc, const int channel)
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
int Packet_mutrig_fpga::iValue(const int ich)
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
int  Packet_mutrig_fpga::iValue(const int ich, const char *what)
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


  else if ( strcmp(what,"BCLCK")==0)
    {
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
	    return 0;
	}
      
      return decoded_data2[4];
    }

   else if ( strcmp(what,"PARITY")==0)
     {
       if (decoded_data2 == NULL )
 	{
 	  if ( (decoded_data2 = decode_misc(&data2_length))==NULL)
 	    return 0;
 	}
      
       return decoded_data2[5];
     }

  return decoded_data2[5];
}



// ------------------------------------------------------

int *Packet_mutrig_fpga::decode (int *nwout)
{
  int *p,*k;


  int dlength = getDataLength();

  p = (int *) findPacketDataStart(packet);
  if (p == 0) 
    {
      *nwout = 0;
      return 0;
    }
  k = &p[5]; // skip the header info


  int nbclk, octant, station, strip;
  
  int index, id, bit29;
  int w;

  cout << endl;

  // bclk=3 * octant=2 * station=3 * strip=20
  int *out = new int (3*2*3*20);
  memset ( out,0, 3*2*3*20*sizeof(int));


  for ( id=0; id < dlength -8; id++ )  // check 8...
    {
      w = k[id];
      if ( ! (w & 0x80000000 ) )
	{
	  index = ( w>>16) & 0xff;
	  bit29 = ( w>>29) & 0x1;
	  //  cout << "bit29 " << bit29 << "  " << setw(5) << index << endl;

	  int i = index % (2*38);  // this  is the offset into one blck regime
	  nbclk = (index -i)/ (2*38); 
	  octant = ( i - ( i%38) ) /38;
	  i = i %38;
	  if ( i<6)       // 6 for station 1
	    {
	      station = 0;
	      strip = i;
	    }
	  else if ( i<18)  // 12 for station 2
	    {
	      station = 1;
	      strip = i-5;;
	    }
	  else if ( i<38)  // 20 for station 3
	    {
	      station = 2;
	      strip = i-17;
	    }
	  else
	    {
	      station = -1;
	      cout << "wrong station " << nbclk << "  " << octant << "  " <<  i << endl;
	    }
	  
	  cout << " coordinates: " << index << "  " << nbclk << "  " << octant << "  " << station << "  " << strip  << endl;


	  out [ nbclk*120  + octant *60 + station*20 + strip] = w & 0xffff; 
	}
    }

  *nwout = 3*2*3*20;
  return out;
}

// ------------------------------------------------------

int *Packet_mutrig_fpga::decode_misc (int *nwout)
{

  int *p,*k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  

  p = new int[7];

  p[0] = k[0] & 0xffff;      // event number
  p[1] = k[1] & 0xffff;      // Flag word 
  p[2] = k[2] & 0xffff;      // Det id       (initially hardwired to 0x1800)
  p[3] = k[3] & 0xffff;        //mod address
  p[4] = k[4] & 0xffff;      // clock number


  *nwout = 7;
  return p;

}

// ------------------------------------------------------

void Packet_mutrig_fpga::dump ( OSTREAM &os) 
{

  //  this->identify(os);
  
  os << " Event number:                    " << iValue(0,"EVTNR") << std::endl; 
  os << " Flag word                        " << std::hex << "0x" << iValue(0,"FLAG")  << std::dec << std::endl;
  os << " Det id                           " << std::hex << "0x" << iValue(0,"DETID") << std::dec << std::endl;
  os << " module ID                        " << std::hex << "0x" << iValue(0,"MODID") << std::dec << std::endl;
  os << " beam clock counter               " << std::hex << "0x" << iValue(0,"BCLCK") << std::dec <<std::endl;

  os << " Parity                           " << std::hex << "0x" << iValue(0,"PARITY") << std::dec <<std::endl;

  int nbclk, octant, station, strip;
  


  for ( nbclk = 0; nbclk < 3; nbclk++)
    {
      for ( octant = 0; octant < 2; octant++)
	{

	  station = 0;
	  for ( strip  = 0; strip < 6; strip++)
	    {  
	      if (iValue(nbclk * octant * station + strip) )
		os << setw(3) << nbclk << setw(3) << octant << setw(3) << station << setw (3) << strip
		   << setw(5) << iValue(nbclk * octant * station + strip) << endl;
	    }

	  station = 1;
	  for ( strip  = 0; strip < 12; strip++)
	    {  
	      if (iValue(nbclk * octant * station + strip) )
		os << setw(3) << nbclk << setw(3) << octant << setw(3) << station << setw (3) << strip
		   << setw(5) << iValue(nbclk * octant * station + strip) << endl;
	    }
	  station = 3;
	  for ( strip  = 0; strip < 20; strip++)
	    {  
	      if (iValue(nbclk * octant * station + strip) )
		os << setw(3) << nbclk << setw(3) << octant << setw(3) << station << setw (3) << strip
		   << setw(5) << iValue(nbclk * octant * station + strip) << endl;
	    }

	}
    }
  //   dumpErrorBlock(os);
  //  dumpDebugBlock(os);

}








