#include <packet_idcstr.h>
#include <stdio.h>

Packet_idcstr::Packet_idcstr(PACKET_ptr data)
  : Packet_w1 (data){}
  


int Packet_idcstr::iValue(const int ich)
{

  if (decoded_data1 == NULL )
    {
      if ( (decoded_data1 = decode(&data1_length))==NULL)
	return 0;
    }

   // see if our array is long enough
  if (ich < 0 || ich >= data1_length) return EOF;

  return decoded_data1[ich];


}



int *Packet_idcstr::decode ( int *nwout)
{
  int *p, *k;
  int i;
  int dlength = getDataLength();

  char* from = (char  *)  findPacketDataStart(packet);
  if ( from == 0) 
    { 
      *nwout = 0;
      return 0;
    }
 
  p = new int[dlength];
  k = p;
  for (i =0; i<dlength; i++) *k++ = from[i];
  *nwout = dlength;
  return p;
}


