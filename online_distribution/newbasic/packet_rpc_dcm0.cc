#include <packet_rpc_dcm0.h>

Packet_rpc_dcm0::Packet_rpc_dcm0()
{
  n_modules = 0;
  expected_length=0;
}

Packet_rpc_dcm0::Packet_rpc_dcm0(PACKET_ptr data)
  : Packet_w4 (data)
{
  n_modules = 0;
  expected_length=0;
}

Packet_rpc_dcm0::~Packet_rpc_dcm0()
{
}

// ------------------------------------------------------


int  Packet_rpc_dcm0::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  // trigger the decoding of it all by asking for a 
  // throwaway iValue(0)
  iValue(0);
  // now we don't have to check for each "what" value

  if ( strcmp(what,"TRIGBIT")==0)
    {
      if (ich < 0 || ich >= expected_length)
	{
	    return 0;
	}
      return decoded_data2[ich];
    }

  else if ( strcmp(what,"MODNR")==0)
    {
      if (ich < 0 || ich >= n_modules)
	{
	    return 0;
	}
      return decoded_data3[ich];
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (ich < 0 || ich >= n_modules)
	{
	    return 0;
	}
      return decoded_data4[ich];
    }

  else if ( strcmp(what,"CLOCK")==0)
    {
      if (ich < 0 || ich >= n_modules)
	{
	    return 0;
	}
      return decoded_data5[ich];
    }



  else if ( strcmp(what,"MODULECOUNT")==0)
    {
      return n_modules;
    }

  else if ( strcmp(what,"CHANNELCOUNT")==0)
    {
      return expected_length;
    }


  return 0;

}



// ------------------------------------------------------

int Packet_rpc_dcm0::iValue (const int strip)
{
    if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }


  // see if our array is long enough
  if (strip >= data1_length || strip < 0) return 0;

  return decoded_data1[strip];
}



int *Packet_rpc_dcm0::decode (int *nwout)
{

  // this one decoding routine will decode several
  // kinds of data. we return the TDC vector as the primary 
  // value. We also hook up 
  //    decoded_data2 -> trigger bits  (one per channel)
  //    decoded_data3 -> moulde address (one per module)
  //    decoded_data4 -> event number (one per module)
  //    decoded_data5 -> Clock number (one per module)


  int *p, *m;
  int *triggerbits;
  int *moduleaddress;
  int *eventnumber;
  int *clocknumber;

    
  int dlength = getDataLength();

  m = (int *) findPacketDataStart(packet);
  if (m == 0) 
    {
      *nwout = 0;
      return 0;
    }
  if ( dlength < 19 ) // that's the minimum length
    {
      *nwout = 0;   
      return 0;
    }

  // each module contributes 19 words, so this tells home many we have
  n_modules = dlength / 19; 

  // the output length is 32 channels per modules
  expected_length = 32 * n_modules;
  
  //we now get the output vector for TDC and trigger bits 
  p             = new int [ expected_length];  
  triggerbits   = new int [ expected_length]; // we decode the trigger bits at the same time
  moduleaddress = new int [n_modules];
  eventnumber   = new int [n_modules];
  clocknumber   = new int [n_modules];


  int i, l;
  int chann = 0;
  int module = 0;
  int im;

  for ( im=0; im<n_modules; im++) // 19 = 16 data words + 3 header 
    {
      i = im * 19;

      if (  ((m[i]>>12) & 0xf) ==4)
	{
	  moduleaddress[module] = m[i] & 0xfff;
	}

      if ( ((m[i+1]>>12) &0xf) ==5)
	{
	  eventnumber[module]  = m[i+1] & 0xfff;
	}

      if ( ((m[i+2]>>12) &0xf) ==6)
	{
	  clocknumber[module] = m[i+2] & 0xfff;
	}

      module++;

      for (l = i+3; l < i+19; l++)
	{
	  triggerbits[chann] = (m[l]>>12) & 0x3; 
	  p[chann] = m[l] & 0x3f; 

	  triggerbits[chann+1] = triggerbits[chann];  // we have the same trigger bits for both channels 
	  p[chann+1] = (m[l] >> 6)  & 0x3f; 
	  chann +=2;
	}
	  

    }

  decoded_data2 = triggerbits;
  data2_length  = chann;

  decoded_data3 = moduleaddress;
  data3_length  = n_modules;

  decoded_data4 = eventnumber;
  data4_length  = n_modules;

  decoded_data5 = clocknumber;
  data5_length  = n_modules;

  *nwout = chann;
  return p;
}

// ------------------------------------------------------

void Packet_rpc_dcm0::dump ( OSTREAM &os) 
{

  
  this->identify(os);
  int mc = iValue(0,"MODULECOUNT");
  int ch = 0;
  int i;

  for ( i = 0; i< mc; i++)  // step through the modules 
    {
      os << "**** module nr: " << iValue(i, "MODNR") 
	 << "  Event number: " << iValue(0,"EVTNR") 
	 << "  Clock: " << iValue(0,"CLOCK") << std::endl; 
    }

  int l;
  for ( l = 0; l < mc * 32; l++)
	{
	  os << std::setw(3) << ch << " | " 
	     << std::setw(2) << iValue (ch, "TRIGBIT") 
	     << std::setw(6) <<  iValue (ch) << std::endl;
	  ch++;
	}
  
}











