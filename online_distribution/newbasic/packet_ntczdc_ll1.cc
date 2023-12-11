#include <packet_ntczdc_ll1.h>
#include <string.h>

Packet_ntczdc_ll1::Packet_ntczdc_ll1(PACKET_ptr data) : Packet_w4 (data)
{
}

int *Packet_ntczdc_ll1::decode (int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_ntczdc_ll1( temp
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

int Packet_ntczdc_ll1::iValue(const int ich, const char *what){
  
  unsigned int *k;

  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet); 
  if (k == 0) return -1;
  if (dlength<4) return -1;

  if(strcmp(what,"HEADER")==0) {
    return (k[0]&0xFFFF);
  }
  else if ( strcmp(what,"NTCVTX")==0)
    {
      char ntc_vertex=(k[1]&0xFF0000)>>16;
      return (int) ntc_vertex;
    }
  else if ( strcmp(what,"ZDCVTX")==0)
    {
      char zdc_vertex=(k[2]&0xFF00)>>8;
      return (int) zdc_vertex;
    }
  else if ( strcmp(what,"ZDCAOK")==0)
    {
      return ((k[1]&0x100)>>8);
    }
  else if ( strcmp(what,"ZDCBOK")==0)
    {
      return ((k[1]&0x200)>>9);
    }
  else if ( strcmp(what,"NTCAOK")==0)
    {
      return ((k[1]&0x40)>>6);
    }
  else if ( strcmp(what,"NTCBOK")==0)
    {
      return ((k[1]&0x80)>>7);
    }
  else if ( strcmp(what,"NTC_MEAN_TDC_N")==0)
    {
      return ((k[1]&0xFE000000)>>25);
    }
  else if ( strcmp(what,"NTC_MEAN_TDC_S")==0)
    {
      return (k[2]&0x7F);
    }
  else if ( strcmp(what,"NTC_HITS_N")==0)
    {
      return ((k[3]&0xF0)>>4);
    }
  else if ( strcmp(what,"NTC_HITS_S")==0)
    {
      return ((k[3]&0xF00)>>8);
    }   
  else if ( strcmp(what,"ZDC_SOUTH_TDC_OK")==0)
    {
      return ((k[2]&0x10000)>>16);
    }
  else if ( strcmp(what,"ZDC_NORTH_TDC_OK")==0)
    {
      return ((k[2]&0x20000)>>17);
    }
  else if ( strcmp(what,"NTC_NORTH_SINGLE")==0)
    {
      // Version 2 or higher
      if(((k[3]&0xF0000)>>16) >= 2 )
        return ((k[1]&0x10)>>4);
      else
	return -1;
    }   
  else if ( strcmp(what,"NTC_SOUTH_SINGLE")==0)
    {
      // Version 2 or higher
      if(((k[3]&0xF0000)>>16) >= 2 )
        return ((k[1]&0x20)>>5);
      else
	return -1;
    }
    

  //  std::cout <<"iValue(int ich, char* what): Unrecognized value "<< what << std::endl;

  return 0;

}

void Packet_ntczdc_ll1::dump ( OSTREAM &os){

  unsigned int *k;
  char oldFill;

  this->identify(os);
  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet);
  if (k == 0) return;

  oldFill=os.fill('0');
  int m;
  for(m=0;m<54;m++) os<<"_";
  os <<std::endl;
	
  os << "NTCZDC LL1 data packet: | ";
  for(int l=0;l<dlength;l++)
  os << std::hex << SETW(8) << k[l] << " | ";
  os << std::endl << std::endl;

  if(dlength>4)
    os<<"Number of words(32) is "<<dlength<<". Only first 4 will be decoded."<<std::endl;
  if(dlength<4)
    os<<"Number of words(32) is "<<dlength<<". Must be 4!"<<std::endl;

  if(dlength==0) return;

  int header=k[0]&0xFFFF;
  os << "Header      = 0x" << std::hex << SETW(4) << header << std::endl;
  os << "CTL Chip ESN= 0x" << std::hex << SETW(1) << ((k[1]&0xC000)>>14) << std::endl;
  os << "ALG Chip ESN= 0x" << std::hex << SETW(1) << ((k[2]&0xC000000)>>26) << std::endl << std::endl;

  os << "CTL Chip Accept Counter= 0x" << std::dec << ((k[0]&0xFF000000)>>24) << std::endl << std::endl;
  
  os << "Control Chip Mode Bits:" << std::endl;
  os << "MODE 1      = 0x" << std::hex << SETW(1) << ((k[0]&0x10000)>>16) << std::endl;
  os << "MODE 5      = 0x" << std::hex << SETW(1) << ((k[0]&0x40000)>>18) << std::endl;
  os << "MODE 6      = 0x" << std::hex << SETW(1) << ((k[0]&0x80000)>>19) << std::endl;
  os << "MODE 7      = 0x" << std::hex << SETW(1) << ((k[0]&0x100000)>>20) << std::endl;
  os << "MODE 8(ENBL)= 0x" << std::hex << SETW(1) << ((k[0]&0x200000)>>21) << std::endl << std::endl;

  if( ((k[3]&0xF0000)>>16) >= 2 ){
    // NTC singles in version 2 and higher
    os << "RB4 (NTCN_single)  = 0x" << std::hex << SETW(1) << ((k[1]&0x10)>>4) << std::endl;
    os << "RB5 (NTCS_single)  = 0x" << std::hex << SETW(1) << ((k[1]&0x20)>>5) << std::endl;
  }
  os << "RB6 (NTCA)  = 0x" << std::hex << SETW(1) << ((k[1]&0x40)>>6) << std::endl;
  os << "RB7 (NTCB)  = 0x" << std::hex << SETW(1) << ((k[1]&0x80)>>7)<< std::endl;
  os << "RB8 (ZDCA)  = 0x" << std::hex << SETW(1) << ((k[1]&0x100)>>8) << std::endl;
  os << "RB9 (ZDCB)  = 0x" << std::hex << SETW(1) << ((k[1]&0x200)>>9) << std::endl;
  os << std::endl;
  
  os << "NTC Alg. Ver= 0x" << std::hex << SETW(1) << ((k[3]&0xF0000)>>16) << std::endl;
  char ntc_vertex=(k[1]&0xFF0000)>>16;
  os << "NTC Vertex  = "<< std::dec << (int) ntc_vertex << std::endl;
  os << "N. Mean TDC = 0x" << std::hex << SETW(2) <<((k[1]&0xFE000000)>>25) << std::endl;
  os << "S. Mean TDC = 0x" << std::hex << SETW(2) << (k[2]&0x7F) << std::endl;
  os << "North Hits  = " << std::dec << ((k[3]&0xF0)>>4) << std::endl;
  os << "South Hits  = " << std::dec << ((k[3]&0xF00)>>8) << std::endl;
  if( ((k[3]&0xF0000)>>16) >= 2 ){
    // NTC singles in version 2 and higher
    os << "North Single  = " << std::dec << ((k[1]&0x1000000)>>24) << std::endl;
    os << "South Single  = " << std::dec << ((k[2]&0x80)>>7) << std::endl;
  }
  os << std::endl;

  os << "ZDC Alg. Ver= 0x" << std::hex << SETW(1) << ((k[2]&0xC00000)>>22) << std::endl;
  char zdc_vertex=(k[2]&0xFF00)>>8;
  os << "ZDC Vertex  = "<< std::dec << (int) zdc_vertex << std::endl;
  os << "ZDC South TDC OK = " << SETW(1) << ((k[2]&0x10000)>>16) << std::endl; 
  os << "ZDC North TDC OK = " << SETW(1) << ((k[2]&0x20000)>>17) << std::endl; 
  os << "ZDCA Vtx.OK = 0x" << std::hex << SETW(1) <<  ((k[2]&0x100000)>>20) << std::endl;
  os << "ZDCB Vtx.OK = 0x" << std::hex << SETW(1) <<  ((k[2]&0x200000)>>21) << std::endl;
  os << std::endl;

  unsigned int mode_bits = ((k[2]&0xF0000000)>>28) + ((k[3]&0xF)<<4);
  os << "ALG Mode Bits = 0x " << std::hex << SETW(2) << mode_bits << std::endl;

  for(m=0;m<54;m++) os<<"_";
  os <<std::endl;
  os.fill(oldFill);

}
