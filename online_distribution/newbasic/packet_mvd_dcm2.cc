#include <packet_mvd_dcm2.h>

Packet_mvd_dcm2::Packet_mvd_dcm2(PACKET_ptr data)
  : Packet_w4 (data){}
  
int *Packet_mvd_dcm2::decode ( int *nwout)
{
  int *p,*k;
  int olength;
  int i;
  int temp[MAX_OUTLENGTH];

  int dlength = getDataLength();

  int status = decode_mvd_dcm2( temp
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
