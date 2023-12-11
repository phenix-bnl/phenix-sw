
#include <packet_rpc_fpga.h>




Packet_rpc_fpga::Packet_rpc_fpga()
{
  n_modules = 0;
  expected_length=0;
}

Packet_rpc_fpga::Packet_rpc_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
  n_modules = 0;
  expected_length=0;
}

Packet_rpc_fpga::~Packet_rpc_fpga()
{
}

// ------------------------------------------------------


int  Packet_rpc_fpga::iValue(const int ich, const char *what)
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
      return decoded_data3[0];  // we have just one header, so only one to give 
    }

  else if ( strcmp(what,"EVTNR")==0)
    {
      if (ich < 0 || ich >= n_modules)
	{
	    return 0;
	}
      return decoded_data3[1];
    }

  else if ( strcmp(what,"CLOCK")==0)
    {
      if (ich < 0 || ich >= n_modules)
	{
	    return 0;
	}
      return decoded_data3[2];
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      return decoded_data3[3];

    }

  else if ( strcmp(what,"PARITYOK")==0)
    {
      return decoded_data3[4];

    }

  else if ( strcmp(what,"EVENTNRMATCH")==0)
    {
      return decoded_data3[5];
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

int Packet_rpc_fpga::iValue (const int strip)
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



int *Packet_rpc_fpga::decode (int *nwout)
{

  // this data format needs this external information
  // is can unfortunatly NOT be general...
  // is is how "fpga position numbers" map to real module numbers
  // so fpga 6 - > module 5, etc


  const int IMOD[32] = {-1, -1, -1, -1,
			-1, -1, 12, 12,
			11, 11, 10, 10, 9, 9, 8, 8,
			7,   7,  6,  6, 5, 5, 4, 4,
			3,   3,  2,  2, 1, 1, 0, 0};



  // this one decoding routine will decode several
  // kinds of data. we return the TDC vector as the primary 
  // value. We also hook up 
  //    decoded_data2 -> trigger bits  (one per channel)
  //    decoded_data3 -> 3 misc. items 
  //    [0] modules address 
  //    [1] event nr 
  //    [2] clock
  //    [3] parity word
  //    [4] parity ok
  //    [5] eventnumber match


  int *p, *m;
  int *triggerbits;
  int *misc;

    
  int dlength = getDataLength();

  m = (int *) findPacketDataStart(packet);
  if (m == 0) 
    {
      *nwout = 0;
      return 0;
    }
  if ( dlength < 3 ) // that's the minimum length
    {
      *nwout = 0;   
      return 0;
    }

  int datawords= 0;

  if (  ( m[dlength-1] &0x80000000))
    { 
      n_modules = 26;
      datawords= ( m[dlength-1] >>23) & 0xf;
      
 
      //      std::cout << std::hex <<  m[dlength-1] << std::dec <<  " nmodules = " << n_modules << " datawords = " 
      //	<< datawords << " found_modules " << found_modules << std::endl;
    }
  else
    {
      *nwout = 0;   
      return 0;
    }
      
  // the output length is 32 channels per modules
  expected_length =  32 * n_modules;
  
  //we now get the output vector for TDC and trigger bits 
  p             = new int [ expected_length];  
  triggerbits   = new int [ expected_length]; // we decode the trigger bits at the same time
  misc = new int [6];

  int i;
  int chann = 0;
  int module_adr = 0;
  int module = 0;

  //  memset ( p,0,expected_length*sizeof(int));

  // don't ask me why, but a suppressed channel is not 0, but 43. oh well. 
  for ( i = 0; i < expected_length ; i++)
    {
      p[i] = 43;
    }

  memset ( triggerbits,0,expected_length*sizeof(int));
  memset ( misc,0,6*sizeof(int));

  
  misc[3] = ( m[dlength-1]  & 0xffff);  // parity word
  misc[4] = ((m[dlength-1] >> 16) & 0x1);  // parity ok
  misc[5] = ((m[dlength-1] >> 17) & 0x1);  // eventnumber match 



  if ( ( m[0] & 0x40000000) ==0x40000000 )  // 1st word
    {
     
      for ( i=0; i<3; i++)
	{
	  if (( m[i] & 0x20000000))
	    {
	      int value = (m[i] & 0xfff);
	      int tag   = (( m[i] >> 12) & 0xf);
	      if ( tag == 0x4)
		{
		  misc[0] = value;  // module address
		  // std::cout << "mod addr = " << std::hex << value << std::dec <<  std::endl;

		}
	      else if ( tag == 0x5)
		{
		  misc[1] = value;  // event number
		  //std::cout << "evt nr  = " << std::hex << value << std::dec <<  std::endl;
		}
	      else if ( tag == 0x6)
		{
		  misc[2] = value;  // clock number
		  // std::cout << "clock = " << std::hex << value << std::dec <<  std::endl;
		}
	    }
	}



      for (i = 3; i < dlength -1 ; i++)
	{

	  module_adr = (( m[i] >> 21) & 0x3f);
	  //std::cout << "mod addr = " <<  module_adr <<  std::endl;
	  if (  module_adr < 32 && module_adr >=6 )
	    {
	      module =  IMOD[ module_adr];
	      chann = (( m[i] >> 16) & 0x1f);
 
	      int real_chann = module*64 + 2*chann;
	  
	      triggerbits[real_chann] = (m[i]>>12) & 0x1; 
	      triggerbits[real_chann+1] = (m[i]>>13) & 0x1; 
	      p[real_chann] = m[i] & 0x3f; 

	      p[real_chann+1] = (m[i] >> 6)  & 0x3f; 

	      //     std::cout << "modulee_adr " << module_adr << " module = " <<  std::dec << module << " chann " 
	      //		<< chann << " realchann " << real_chann
	      //		<<  " tdc = " << p[real_chann] << "  " << p[real_chann+1] << std::endl;
	    }
	}

    }

  decoded_data2 = triggerbits;
  data2_length  = chann;

  decoded_data3 = misc;
  data3_length  = 5;

  *nwout = n_modules*32;
  return p;
}

// ------------------------------------------------------

void Packet_rpc_fpga::dump ( OSTREAM &os) 
{

  
  this->identify(os);
  int mc = iValue(0,"MODULECOUNT");
  int m;
  int ch = 0;

  os << " Module addr:   " << iValue(0,"MODNR") << std::endl;
  os << " Event Nr:      " << iValue(0,"EVTNR") << std::endl;
  os << " Clock:         " << iValue(0,"CLOCK") << std::endl;
  os << " Parity:        " << iValue(0,"PARITY") << std::endl;
  os << " Parity ok:     " << iValue(0,"PARITYOK")  << std::endl;
  os << " EvtNr Match:   " << iValue(0,"EVENTNRMATCH") << std::endl;
  os << " Nr of Modules  " << mc << std::endl;
  os << " Nr of Channels " << iValue(0,"CHANNELCOUNT") << std::endl;
     

  int l;
  for ( m = 0; m < mc; m++)
    {

      for ( l = 0; l <  32; l++)
	{
	  os << std::setw(3) << ch << " | " 
	     << std::setw(2) << iValue (ch, "TRIGBIT") 
	     << std::setw(6) <<  iValue (ch) << std::endl;
	  ch++;
	}
    }
  
}


