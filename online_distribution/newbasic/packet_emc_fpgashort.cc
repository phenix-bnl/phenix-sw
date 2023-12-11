#include <packet_emc_fpgashort.h>

Packet_emc_fpgashort::Packet_emc_fpgashort(PACKET_ptr data)
  : Packet_emc_fpga (data){};
  
int *Packet_emc_fpgashort::decode ( int *nwout)
{


  int *k;
  int temp[MAX_OUTLENGTH];
  memset(temp,0,MAX_OUTLENGTH*sizeof(int));

  k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      *nwout = 0;
      return 0;
    }


  int dlength = getDataLength();

  int pos=8;
  int ch, ch1,ch2, tflag;

  // find all channel data
  int olength = 0;
  int currentmaxlen;
  while (pos <= dlength-3) 
    {

      //      if ((k[pos] & 0xffff0000) == 0x10080000) break; // indicates end of data words

      ch = (k[pos] >> 24 ) & 0xff;
      currentmaxlen = ch*5+4+1; // maximum array index for current channel
      if (currentmaxlen >= MAX_OUTLENGTH)
	{
          *nwout = 0;
          return 0;
	}
      if (currentmaxlen > olength)
	{
	  olength = currentmaxlen;
	}

      ch1 = (k[pos+1] >> 24 ) & 0xff;  // the channel number encoded in 2nd word
      ch2 = (k[pos+2] >> 20 ) & 0xff; // the channel number encoded in 3rd word (both should match ch) 
      tflag = (k[pos+2] >> 16 ) & 0xf;  // the marker in the timing word, should be 0xd
      
      if ( ch != ch1 || ch != ch2 ||  ch1 != ch2 || tflag != 0xd )
	{
	  std::cout << __FILE__ << " " << __LINE__ << "inconsistenr chan numbers " 
		    << ch << "  " << ch1 << "  " << ch2 << " 0x"<< std::hex << tflag << std::dec << std::endl;
	} 

      temp[ch*5+3]  =   k[pos] & 0xfff; //high gain pre
      temp[ch*5+1]  =  (k[pos] >> 12) & 0xfff; //high gain post
      temp[ch*5+4]  =  k[pos+1] & 0xfff;   // low gain pre
      temp[ch*5+2]  =  (k[pos+1] >> 12) & 0xfff; //low gain post
      temp[ch*5+0]  =  k[pos+2] & 0xfff;   //time
      hitlist [hitlength++] = ch;


      pos+=3;
    }
  
  int *p = new int[olength];
  int *kk = p;
  for (int i =0; i<olength; i++) 
    {
      *kk++ = temp[i];
    }
  *nwout = olength;
  return p;
}

int Packet_emc_fpgashort::decode_to_sparse ( int *p, const int nlen )
{

  int dlength = getDataLength();

  int *k = (int *) findPacketDataStart(packet);
  if (k == 0) 
    {
      return 0;
    }


  int pos=8;
  int count=0;
  int index=0;

  // find all channel data
  while (pos  <  dlength -3) 
    {
      //      if ((k[pos] & 0xffff0000) == 0x10080000) break; // indicates end of data words

      p[index+0] = (k[pos] >> 24 ) & 0xff;
      p[index+1] = k[pos+2 ] & 0xfff;         // time  
      p[index+2] = (k[pos ]>>12)  & 0xfff;    // highpost
      p[index+3] = (k[pos+1 ] >>12) & 0xfff;  // lowpost
      p[index+4] = k[pos ] & 0xfff;           // highpre
      p[index+5] = k[pos+1 ] & 0xfff;         // lowpre

      pos+=3;
      count++;
      index +=6;
    }

  return count;
}

