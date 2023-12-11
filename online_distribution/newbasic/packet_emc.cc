#include <packet_emc.h>


Packet_emc::Packet_emc(PACKET_ptr data)
  : Packet_w4 (data){max_channels=0;}
  
// ------------------------------------------------------

int  Packet_emc::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

// ------------------------------------------------------

  if (strcmp(what,"AMU") == 0)  // user requested AMU cells info
    {			
      if (decoded_data2 == NULL )
	{
	  if ( (decoded_data2 = decode_amu(&data2_length))==NULL)
	    return 0;
	}
      if (ich > data2_length) return 0;

      return decoded_data2[ich];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}
      return decoded_data3[0];
    }

  else if ( strcmp(what,"USERWORD")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      if (ich > 7 || ich < 0) return 0;

      return decoded_data3[3+ich];
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[1];
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[2];
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      if (decoded_data3 == NULL )
	{
	  if ( (decoded_data3 = decode_misc(&data3_length))==NULL)
	    return 0;
	}

      return decoded_data3[11];
    }


  else	return 0;
}


// ------------------------------------------------------

int   Packet_emc::iValue(const int ich, const int iy)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

  // see if our array is long enough
  if (ich >= max_channels) return 0;
  if (iy >= 5) return 0;
  return decoded_data1[ich*5 + iy];
}

// ------------------------------------------------------









