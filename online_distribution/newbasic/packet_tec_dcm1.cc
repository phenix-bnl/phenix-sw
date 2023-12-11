#include <packet_tec_dcm1.h>

Packet_tec_dcm1::Packet_tec_dcm1(PACKET_ptr data)
  : Packet_tec_dcm0 (data){}
  

// ------------------------------------------------------

int  Packet_tec_dcm1::iValue(const int ich, const char *what)
{
  // now let's derefence the proxy array. If we didn't decode
  // the data until now, we do it now

  int *k;
  int dlength = getDataLength();
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }

  if ( strcmp(what,"EVTNR")==0)
    {
      return k[1] & 0xfffff;  // Event number
    }

  else if ( strcmp(what,"MODULE")==0)
    {
      return k[2] & 0xfffff;
    }

  else if ( strcmp(what,"FLAGS")==0)
    {
      return k[3] & 0xfffff;
    }

  else if ( strcmp(what,"BCLK")==0)
    {
      return k[4] & 0xfffff;
    }

  else if ( strcmp(what,"USERWORD")==0)
    {
      return k[dlength-3]; 
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      return k[dlength-2]; 
    }

  else	return 0;
}

// ------------------------------------------------------

void Packet_tec_dcm1::dump ( OSTREAM &os) 
{
  int *k;
  
  this->identify(os);
  
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return;
    }
  
  os << "  Detector id:        "  << SETW(8) << std::hex << (k[0] &0xfffff) << std::dec << std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[1] &0xfffff) << std::dec << std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[2] &0xfffff) << std::dec << std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[3] &0xfffff) << std::dec << std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[4] &0xfffff) << std::dec << std::endl;    // Beam Clock Counter

  int * packetData = (int *) findPacketDataStart (packet);

  int j = 5;
  int l;
  while (1)
    {
      os << SETW(5) << std::dec << j << " |  ";
      for (l=0;l<4;l++)
	{
	  os << std::hex << SETW(8) << packetData[j++] << " " ;
	  if (j>=getDataLength() ) break;
	}
      if (j>=getDataLength() ) break;
      os << std::endl;
    }
  os << std::endl;

   dumpErrorBlock(os);
   dumpDebugBlock(os);
  
}
