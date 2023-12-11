
#include <packet_dch_dcm0.h>

Packet_dch_dcm0::Packet_dch_dcm0(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_dch_dcm0::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_dch_dcm0( temp
			      ,(int *)  findPacketDataStart(packet) 
			      ,dlength
			      ,MAX_OUTLENGTH, &olength);

  if (status || olength<=0 ) return NULL;
 
  p = new int[olength];
  k = p;
  for (i =0; i<olength; i++) *k++ = temp[i];
  *nwout = olength;
  return p;
}
// ------------------------------------------------------

int  Packet_dch_dcm0::iValue(const int ich, const char *what)
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

  else if ( strcmp(what,"BCLK")==0)
    {
      return k[4] & 0xffff;
    }

  else if ( strcmp(what,"PARITY")==0)
    {
      return k[dlength-2] & 0xfffff; 
    }


 else if ( strcmp(what,"CHECKPARITY")==0)
    {


      int my_parity = 0;
      int *k = (int *) findPacketDataStart(packet);
      if ( ! k) return 0;
      int j=0;
      while (1)
	{
	  my_parity ^= (k[j++] & 0xFFFFF);
	  if ( j>= getDataLength()-2 ) break;
	}
      if ( my_parity != iValue(0,"PARITY")) return -1;
      else return 1;
      
    }




  else	return 0;
}

// ------------------------------------------------------

void Packet_dch_dcm0::dump ( OSTREAM &os) 
{
  int *k;
  
  this->identify(os);
  
  
  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return ;
    }

  
  //  os << "  Start marker word : "  << SETW(8) << std::hex << (k[0] &0xfffff) << std::dec <<std::endl;             //  ???
  os << "  Detector id:        "  << SETW(8) << std::hex << (k[0] &0xfffff) << std::dec <<std::endl;             //  ???
  os << "  Event number:       "  << SETW(8) << std::hex << (k[1] &0xfffff) << std::dec <<std::endl;    // Event number
  os << "  Module address:     "  << SETW(8) << std::hex << (k[2] &0xfffff) << std::dec <<std::endl;             // Module Address
  os << "  Flag Word:          "  << SETW(8) << std::hex << (k[3] &0xfffff) << std::dec <<std::endl;             //  ???
  os << "  Beam Clock Counter: "  << SETW(8) << std::hex << (k[4] &0xfffff) << std::dec <<std::endl;    // Beam Clock Counter

  int * packetData = (int *) findPacketDataStart (packet);

  int j = 6;
  int l;
  while (1)
    {
      os << SETW(5) << std::dec << j << " |  ";
      for (l=0;l<4;l++)
	{
	  os << std::hex << SETW(8) << packetData[j++] << " " << std::dec;
	  if (j>=getDataLength() ) break;
	}
      if (j>=getDataLength() ) break;
      os <<std::endl;
    }
  os <<std::endl;
  
 os << " --- Parity Check: " << iValue(0,"CHECKPARITY") <<std::endl;
 
   dumpErrorBlock(os);
   dumpDebugBlock(os);
}
