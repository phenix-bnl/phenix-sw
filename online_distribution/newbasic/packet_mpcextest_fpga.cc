#include <packet_mpcextest_fpga.h>

using namespace std;

Packet_mpcextest_fpga::Packet_mpcextest_fpga()
{
  preset();
}

Packet_mpcextest_fpga::Packet_mpcextest_fpga(PACKET_ptr data)
  : Packet_w4 (data)
{
  preset();
}

int Packet_mpcextest_fpga::preset()
{
  memset ( chip_enabled,0, CHIPS*CHAINS*sizeof(int));
  memset ( cellnumber,     0, CHIPS*CHAINS*sizeof(int) );
  memset ( nr_chips,    0, CHAINS*sizeof(int) );
  memset ( chips,       0, 128*CHIPS*CHAINS*sizeof(int) );

  evt_nr = 0;
  CBtestMode =0;
  FEMTestMode =0;
  graydecoding = 0;
  chainmask=0;
  maxnrchips = 0;
  detid=0;
  femnr=0;
  bclk=0;
  bclk_extended=0;
  firmwareversion=0;
  headerlength=0;
  trailerlength=0;
  memset (BEM, 0, 4*sizeof(int) );

  PARstTIME = 0;
  TrigPhase = 0;
  EOEStamp = 0;
  CRC=0;
  Stack = -1;
  StatePhase = -1;



  parityok =0;
  parity_comparison =0;
  calculated_parity =0;
  parity_is_calculated = 0;
  _error =0;
  is_decoded = 0;
  return 0;

}


Packet_mpcextest_fpga::~Packet_mpcextest_fpga()
{
}

// ------------------------------------------------------

// we think in terms of SVX4 chips with 128 channels

int  Packet_mpcextest_fpga::iValue(const int channel,  const int chain)
{
  if ( chain < 0 || chain >3) return 0;
  if ( channel < 0 || channel >= CHIPS * 128) return 0;

  int chip = channel / 128;
  int c = channel%128;

  decoded_data1 = decode(&data1_length);
  return chips[c][chip][chain];

}

// ------------------------------------------------------
int Packet_mpcextest_fpga::iValue(const int ich)
{

  if ( ich < 0 || ich >= CHIPS*CHAINS*128) return 0;
  int chain = ich / ( CHIPS*128);
  int c = ich %  ( CHIPS*128);

  return iValue(c,chain);
}


// ------------------------------------------------------
int  Packet_mpcextest_fpga::iValue(const int chip, const int chain, const char *what)
{
  // all those are keyed by the chain. If chain is not 0...3, return 0

  if ( chain < 0 || chain >= CHAINS) return 0;
  
  decoded_data1 = decode(&data1_length);


  if ( strcmp(what,"CELLNR")==0)
    {
      if ( chip < 0 || chip >= CHIPS) return 0;

       decoded_data1 = decode(&data1_length);

      return cellnumber[chip][chain];
    }

  return 0;
}


// ------------------------------------------------------
int  Packet_mpcextest_fpga::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

 decoded_data1 = decode(&data1_length);


  if ( strcmp(what,"EVTNR")==0)
    {
      return  evt_nr;
    }


  else if ( strcmp(what,"CBTESTMODE")==0)
    {
      return CBtestMode;
    }

  else if ( strcmp(what,"FEMTESTMODDE")==0)
    {
      return FEMTestMode;
    }

  else if ( strcmp(what,"GRAYDECODING")==0)
    {
      return graydecoding;
    }

  else if ( strcmp(what,"MAXNRCHIPS")==0)
    {
      return maxnrchips;
    }

  else if ( strcmp(what,"DETID")==0)
    {
      return detid;
    }

  else if ( strcmp(what,"FEMID")==0)
    {
      return femnr;
    }

  else if ( strcmp(what,"BCLCK")==0)
    {
      return bclk;
    }

  else if ( strcmp(what,"EXTENDEDBCLCK")==0)
    {
      return bclk_extended;
    }

  else if ( strcmp(what,"FIRMWARE")==0)
    {
      return firmwareversion;
    }

  else if ( strcmp(what,"PARSTTIME")==0)
    {
      return PARstTIME;
    }

  else if ( strcmp(what,"TRIGGERPHASE")==0)
    {
      return TrigPhase;
    }

  else if ( strcmp(what,"HFSTAT")==0)
    {
      return HFStat;
    }

  else if ( strcmp(what,"STACK")==0)
    {
      return Stack;
    }

  else if ( strcmp(what,"STATEPHASE")==0)
    {
      return StatePhase;
    }

   else if ( strcmp(what,"PARITY")==0)
     {
       calculate_parity();
       return CRC;
     }

   else if ( strcmp(what,"CALCULATEDPARITY")==0)
     {
       calculate_parity();
       return calculated_parity;
     }

  else if ( strcmp(what,"CHECKPARITY")==0)
    {
      if (! parity_comparison  )
	{
	  calculate_parity();
	}
      return parity_comparison;
    }


  return 0;
}



// ------------------------------------------------------

int *Packet_mpcextest_fpga::decode (int *nwout)
{


  if ( is_decoded) return 0;

  int *k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }
  
  int index, cindex, h_index, bit29;

  unsigned int *trailerstart = 0;
  int trailerindex = 0;

  int dlength = getDataLength();
  // now we look for End of Event marker
  for ( int i = dlength; i >= dlength-5; i--)
    {
      if ( (k[i] & 0xffff) == 0xFE0D )
	{
	  trailerstart = (unsigned int *) &k[i];
	  trailerindex = i;
	}
    }

  evt_nr = k[0] & 0xffff;

  // we start with word 2, since depending on that value, things can change ---------------
  detid = k[2] & 0xffff;  // should be 0x f0c1 or foc2


  if ( detid == 0xf0c2 )  // new design on 3/18/15 by TKH
    {

      Stack = (k[1]>>12) & 0xf;
      StatePhase = k[1] & 0xfff;

      // word 3 now  ----------
      chainmask  = (k[3] >> 8) & 0xf;  // chainmask has been moved here

      maxnrchips = 0xC; // constant now? was (k[1] >> 4) & 0xf;

      // [13:13]=0 CBTest mode: Fake data from carrier boards
      if (  k[3] & 0x2000 )
	{
	  CBtestMode =1;
	}
    
      // [14:14]=0 FEMTest mode: Fake data from FEM
      if (  k[3] & 0x4000 )
	{
	  FEMTestMode =1;
	}
    
      // [15:15]=0 Gray: Disable Gray decoding
      if (  k[3] & 0x8000 )
	{
	  graydecoding =1;
	}

      // word 3 ---------------
      femnr = k[3] & 0xff;
    }
  else // the originial design before a change 3/18/15
    {
      // word 1  ----------
      chainmask  = (k[1]) & 0xf;
      maxnrchips = (k[1] >> 4) & 0xf;

      // [13:13]=0 CBTest mode: Fake data from carrier boards
      if (  k[1] & 0x2000 )
	{
	  CBtestMode =1;
	}
    
      // [14:14]=0 FEMTest mode: Fake data from FEM
      if (  k[1] & 0x4000 )
	{
	  FEMTestMode =1;
	}
    
      // [15:15]=0 Gray: Disable Gray decoding
      if (  k[1] & 0x8000 )
	{
	  graydecoding =1;
	}

      // word 3 ---------------
      femnr = k[3] & 0xff;
    }

  // word 4 ---------------
  bclk  = k[4] & 0xffff;

  // word 5 ---------------
  firmwareversion = (k[5]>>8) & 0xff;
  headerlength    = (k[5]) & 0xf;
  trailerlength = (k[5]>> 4) & 0xf;

  //  cout <<  "chainmask , maxnr chips " << hex << chainmask << dec <<"  " << maxnrchips << endl;
  //  cout <<  "headerlength , trailer length " << headerlength << "  " << trailerlength<< endl;

  // word 6 ---------------
  PARstTIME = k[6] & 0xffff;

  // word 7 ---------------
  TrigPhase = k[7] & 0xf;
  bclk_msb = (k[7] >>4) & 0xfff;   // 12 MSB of bclk
  bclk_extended = (bclk_msb<<16) | bclk;

  // word 8 ---------------
  BEM[0] = (k[8]>>6) & 0x3f;
  BEM[1] = k[8] & 0x3f;
  HFStat = (k[8]>>12) & 7;   // is this right?

  // word 9 ---------------
  if ( headerlength >9)  // if we have an "extended header"
    {
      BEM[2] = (k[9]>>6) & 0x3f;
      BEM[3] = k[9] & 0x3f;
    }
  //  cout <<  "BEM " << hex << BEM[0]  << "  " << BEM[1]  << "  " << BEM[2]  << "  " << BEM[3]  << "  " << dec << endl;


  //we look at the number of chips
  int i, chain;

  for ( chain = 0; chain < 4; chain++)
    {
      nr_chips[chain] = 0;

      for ( i = 0; i < 6; i++) 
  	{
   	  if ( (BEM[chain] >>i) & 1 ) 
   	    {
   	      nr_chips[chain]++;
   	    }
   	}
    }

  //  cout << hex << "BEM 0 " << BEM[0] << dec << endl;

  int chip;

  for ( chain = 0; chain < 4; chain++)
    {
      for ( chip = 0; chip < CHIPS; chip++) 
	{
	  if ( (BEM[chain] >>chip) & 1 ) 
	    {
	      chip_enabled[chip][chain] =1;
	      //	      cout <<  "enabled " << chip << "  " << chain   << "  " << chip_enabled[chip][chain] << endl;
	    }
	}
    }

  // now the trailer
  if ( trailerstart) 
    {
      EOEStamp = trailerstart[0] & 0xffff;
      CRC  = trailerstart[1] & 0xffff;
    }

  // now e are actually going to the payload, which starts at the
  // index 10
  int *d = &k[10];

  //= (( (*d) >> 16) & 0xffff) - 0xa;

  const int svx4block = 2 + 2 * 128;

  for ( index = 0; index < trailerindex -10; index++ ) 
    {
      // this is the way we calculate the index - it starts with 
      // 
      h_index = ( (d[index]  >> 16) & 0x1fff);
      bit29 = ( (d[index]  >> 29) & 0x1);
      
      if ( bit29)
	{
	  chip = (h_index -10) / svx4block;
	  cindex = (h_index-10) % svx4block;
	}
      else 
	{
	  chip = (h_index) / (2*128);  //instead of "svx4block" (256 + 2), 
	  cindex = h_index % (2*128);  // this index just counts in terms of 256

	}
      //      cout << "index " << index << " value "  << hex << d[index] << dec << " chip " << chip << " cindex " << cindex << " "; 


      // keep in mind that the SVX4 chip brings up the CELL ID in binary. It gets converted
      // from gray to binary, wrongly assuming it is a gray number. So we need to revert it *back* 
      // to "gray" - its originial form.

      if ( bit29 )  // we have a cell id
	{
	  cellnumber[chip][2*cindex] = binaryToGray ( (d[index]>>8) & 0xff);
	  cellnumber[chip][2*cindex+1] = binaryToGray( (d[index]) & 0xff);
	  //	  cout << " cellnumber " << cellnumber[chip][2*cindex] << " " << cellnumber[chip][2*cindex+1] << " index " << hex << h_index << dec << endl;
	}
      else
	{
	  int ichannel = cindex /2;
	  if ( (cindex & 1) == 0 ) // if it is an even number, we have chains 0 and 1
	    {
	      chips[ichannel][chip][0] = (d[index]>>8) & 0xff;
	      chips[ichannel][chip][1] = (d[index]) & 0xff;
	      //  cout << " value 0 1  " << chips[ichannel][chip][0] << " " << chips[ichannel][chip][1] << endl;
	    }
	  else   // else we have chains 2 and 3
	    {
	      chips[ichannel][chip][2] = (d[index]>>8) & 0xff;
	      chips[ichannel][chip][3] = (d[index]) & 0xff;
	      //cout << " value 2 3  " << chips[ichannel][chip][2] << " " << chips[ichannel][chip][3] << endl;
	    }
	}
    }

  is_decoded = 1;

  return 0;

}

      

  // for ( chip = 0; chip < CHIPS; chip++)
  //   {
      
  //     cellnumber[chip][0] = (d[index]>>8) & 0xff;
  //     cellnumber[chip][1] = (d[index]) & 0xff;
  //     index++;

  
  //     cellnumber[chip][2] = (d[index]>>8) & 0xff;
  //     cellnumber[chip][3] = (d[index]) & 0xff;
  //     index++;

  //     for (i = 0; i < 128; i++)
  // 	{
  // 	  chips[i][chip][0] = (d[index]>>8) & 0xff;
  // 	  chips[i][chip][1] = (d[index]) & 0xff;
  // 	  index++;
  // 	  chips[i][chip][2] = (d[index]>>8) & 0xff;
  // 	  chips[i][chip][3] = (d[index]) & 0xff;
  // 	  index++;
  // 	}
  //   }



// ------------------------------------------------------

int Packet_mpcextest_fpga::calculate_parity ()
{

  if ( parity_is_calculated) return 0;

  decoded_data1 = decode(&data1_length);

  int *k;

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }

  int dlength = getDataLength();

  int i;
  int p = 0;

  for (i=0; i< dlength -2; i++)
    {
      p = p ^ (k[i] & 0xffff);
      //      cout << i << "  " << std::hex << p << std::dec << std::endl;
    }

  calculated_parity = p;

  if ( getHitFormat()  == IDMPCEX_FPGA0SUP)
    {
      parity_comparison = 0;
    }
  else if( p == CRC )
    {
      parity_comparison = 1;
    }
  else
    {
      parity_comparison = -1;
    }
  parity_is_calculated = 1;
  return 0;
}


// ------------------------------------------------------

void Packet_mpcextest_fpga::dump ( OSTREAM &os) 
{

  int chip;
  int chain;

  this->identify(os);
  
  os << " Event number:                   " << iValue(0,"EVTNR") << std::endl;

  os << " Det id                          " << std::hex << "0x" << iValue(0,"DETID") << std::dec << std::endl;
  os << " Firmware                        " << std::hex << "0x" << iValue(0,"FIRMWARE") << std::dec << std::endl;
  os << " FEM ID                          " << std::hex << "0x" << iValue(0,"FEMID") << std::dec << std::endl;
  os << " Beam Clock Counter              " << std::hex << "0x" << iValue(0,"BCLCK") << std::dec <<std::endl;
  os << " Extended Beam Clock Counter     " << std::hex << "0x" << iValue(0,"EXTENDEDBCLCK") << std::dec <<std::endl;
  os << " PreAmp Reset distance (ticks)   " << std::hex << "0x" << iValue(0,"PARSTTIME") << std::dec <<std::endl;
  os << " Stack                           " << std::hex << "0x" << iValue(0,"STACK") << std::dec <<std::endl;
  os << " State Phase                     " << std::hex << "0x" << iValue(0,"STATEPHASE") << std::dec <<std::endl;
  os << " Parity                          0x" << std::hex << iValue(0,"PARITY");
  if ( iValue(0,"CHECKPARITY") == 1)
    {
      os << " Passed" << std::dec <<std::endl;
    }
  else if ( iValue(0,"CHECKPARITY") == 0)
    {
      os << " - Match cannot be calculated" << std::dec <<std::endl;
    }
  else
    {
      os << " Failed - calculated parity 0x" << iValue(0, "CALCULATEDPARITY")  << std::dec <<std::endl;
    }
      
  os << " CB Test Mode                   ";
  if ( iValue(0,"CBTESTMODE"))
    {
      os << " Yes" <<std::endl;
    }
  else
    {
      os << " No" << std::endl;
    }

  os << " FEM Test Mode                  ";
  if ( iValue(0,"FEMTESTMODE"))
    {
      os << " Yes" <<std::endl;
    }
  else
    {
      os << " No" << std::endl;
    }

  os << " GRAY Decoding                  ";
  if ( iValue(0,"GRAYDECODING"))
    {
      os << " Yes" <<std::endl;
    }
  else
    {
      os << " No" << std::endl;
    }




  os << "----------------------------------------------------------------------------------------" << endl;
  os << endl;

  os << "                             Cell Nr:" << endl;
  os << " Chain | Chip  0    1    2    3    4    5    6    7    8    9   10   11" << endl;
  os << "-----------------------------------------------------------------------" << endl;

  for ( chain = 0; chain < CHAINS; chain++)
    {
      os << " " << std::setw(3) << chain << "   |   ";

      for ( chip = 0; chip < CHIPS; chip++)
	{
	  os << " " << std::setw(4) <<  iValue(chip,chain,"CELLNR");
	}
      os << endl;
    }
  
  os << endl;
  
  
  int channel_row;
  int c;
  
  
  os << "                               Chain 0                              Chain 1                             Chain 2                             Chain 3" << endl;
  
  for ( chip = 0; chip < CHIPS; chip++)
    {
      for ( channel_row = 0; channel_row < 128; channel_row+= 8)
	{
	  
	  if ( ! channel_row)
	    {
	      os << " " << "Chip"<< std::setw(3)  << chip  << "  ";
	    }
	  else 
	    {
	      os << "          ";
	    }
	  
	  for ( chain = 0; chain < CHAINS; chain++)
	    {
	      if ( ! chain)
		{
		  os << " " << std::setw(3) << channel_row << " |   ";
		}
	      for ( c = channel_row; c < channel_row+8; c++)
		{
		  os << " " << std::setw(3) <<  iValue(chip*128+c,chain);
		}
	      os << "    ";
	    }
	  os << endl;
	  
	}
      os << endl;
      
    }
  
  
  os << "----------------------------------------------------------------------------------------" << endl;
  os << endl;
  
  
}


int Packet_mpcextest_fpga::binaryToGray(int num)
{
  return (num >> 1) ^ num;
}


int Packet_mpcextest_fpga::grayToBinary(int num)
{
   int mask;
  for (mask = num >> 1; mask != 0; mask = mask >> 1)
    {
      num = num ^ mask;
    }
  return num;
}







