#include <packet_bbc_ll1.h>
#include <string.h>

Packet_bbc_ll1::Packet_bbc_ll1(PACKET_ptr data)
  : Packet_w4 (data){}
  
int Packet_bbc_ll1::iValue(const int ich, const char *what)
{
  unsigned int *k;

  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet); 
  if (k == 0) 
    {
      return 0;
    }


  if(dlength<3) return 0;

  int header=k[0]&0xFFFF;

  short vertex=k[1]&0xFF80;
  vertex>>=7;
  //  if(k[1]&8000) vertex=-vertex;

  int time  =(k[0]&0x7F800000)>>23;
  int NHits=(k[0]&0x7F0000)>>16;
  int SHits=k[1]&0x7F;
  int hitsIn=(k[1]&0x7FF0000)>>16;
  int bbcRB=(k[1]&0x18000000)>>27;
  int verOK=(k[1]&0x20000000)>>29;
  int mode=k[2]&0x1FF;

  if ( strcmp(what,"HEADER")==0)
    {
      return header;
    }
  else if ( strcmp(what,"VERTEX")==0)
    {
      return vertex;
    }
  else if ( strcmp(what,"TIME")==0)
    {
      return time;
    }
  else if ( strcmp(what,"NHITS")==0)
    {
      return NHits;
    }
  else if ( strcmp(what,"SHITS")==0)
    {
      return SHits;
    }
  else if ( strcmp(what,"HITSIN")==0)
    {
      return hitsIn;
    }
  else if ( strcmp(what,"GL1RB")==0)
    {
      return bbcRB;
    }
  else if ( strcmp(what,"VTXOK")==0)
    {
      return verOK;
    }
  else if ( strcmp(what,"MBITS")==0)
    {
      return mode;
    }

  else return 0;

}

int *Packet_bbc_ll1::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int temp[MAX_OUTLENGTH];
  int i;
  int dlength = getDataLength();

  int status = decode_bbc_ll1( temp
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

void Packet_bbc_ll1::dump (OSTREAM &os) 
{
  unsigned int *k;
  char oldFill;

  this->identify(os);
  int dlength = getDataLength();
  k= (unsigned int *) findPacketDataStart(packet);

  if ( k == 0) return;

  oldFill=os.fill('0');
  int m;
  for(m=0;m<54;m++) os<<"_";
  os << std::endl;
	
  os << "BB LL1 data packet: | ";
  for(int l=0;l<dlength;l++)
  os << std::hex << SETW(8) << k[l] << " | ";
  os <<std::endl;

  if(dlength>3)
    os<<"Number of words(32) is "<<dlength<<". Only first 3 will be decoded."<< std::endl;
  if(dlength<3)
    os<<"Number of words(32) is "<<dlength<<". Must be 3!"<< std::endl;
  if(dlength==0) return;
  int header=k[0]&0xFFFF;
  os << "Header      =0x" << std::hex << SETW(4) << header <<std::endl;
  short vertex=k[1]&0xFF80;
  vertex>>=7;
  //  if(k[1]&8000)vertex=-vertex;
  os << "Vertex      ="<< std::dec << vertex <<std::endl;
  int time  =(k[0]&0x7F800000)>>23;
  os << "Average     ="<< std::dec << SETW(3) << time <<std::endl;
  if(dlength<2) return;
  int NHits=(k[0]&0x7F0000)>>16;
  os << "North hits  ="<< std::dec << SETW(2) << NHits <<std::endl; 
  int SHits=k[1]&0x7F;
  os << "South hits  ="<< std::dec << SETW(2) << SHits <<std::endl; 
  int hitsIn=(k[1]&0x7FF0000)>>16;
  os << "Hits Input  =0x"<< std::hex << SETW(3) << hitsIn <<std::endl; 
  int bbcRB=(k[1]&0x18000000)>>27;
  os << "BB LL1 RB   ="<< std::dec << SETW(1) << bbcRB <<std::endl; 
  int verOK=(k[1]&0x20000000)>>29;
  os << "Vertex OK   ="<< std::dec << SETW(1) << verOK <<std::endl; 
  if(dlength<3) return;
  int mode=k[2]&0x1FF;
  os << "Mode Bits   =0x"<< std::hex << SETW(3) << mode <<std::endl;
  for(m=0;m<54;m++) os<<"_" << std::dec;
  os << std::endl;
  os.fill(oldFill);
}











