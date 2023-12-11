#include <packet_emc_fpga3words.h>

Packet_emc_fpga3words::Packet_emc_fpga3words(PACKET_ptr data)
  : Packet_emc_fpga (data){};
  
int *Packet_emc_fpga3words::decode ( int *nwout)
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
  int ch;

  // find all channel data
  int olength = 0;
  int currentmaxlen;
  while (pos <= dlength-3) 
    {

      if ((k[pos] & 0xc0000000) == 0xc0000000) // marker byte
	{
	  
	  ch = (k[pos] >> 20 ) & 0xff;

	  if ( ch >= 0 && ch < 144 )
	    {
	      
	      
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
	  

// 	      temp[ch*5+1]  =  ((k[pos] ) & 0xfff) ^ 0xfff; //high gain post
// 	      temp[ch*5+2]  =  ((k[pos+1] >> 12) & 0xfff)  ^ 0xfff; //low gain post
// 	      temp[ch*5+3]  =  ((k[pos+1] ) & 0xfff) ^ 0xfff ; //high gain pre
// 	      temp[ch*5+0]  =  ((k[pos+2] >>12) & 0xfff) ^ 0xfff;   //time
// 	      temp[ch*5+4]  =  ((k[pos+2]) & 0xfff) ^ 0xfff;   // low gain pre

	      temp[ch*5+1]  =  ((k[pos] ) & 0xfff); //high gain post
	      temp[ch*5+2]  =  ((k[pos+1] >> 12) & 0xfff); //low gain post
	      temp[ch*5+3]  =  ((k[pos+1] ) & 0xfff) ; //high gain pre
	      temp[ch*5+0]  =  ((k[pos+2] >>12) & 0xfff);   //time
	      temp[ch*5+4]  =  ((k[pos+2]) & 0xfff) ;   // low gain pre
	  
	      hitlist [hitlength++] = ch;
	    }
	  pos+=3;
	}
      else  // we didn't find the marker byte and go into search mode
	{
	  pos++;
	}
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

int Packet_emc_fpga3words::decode_to_sparse ( int *p, const int nlen )
{

  int dlength = getDataLength();

  int ch;
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

      if ((k[pos] & 0xc0000000) == 0xc0000000) // marker byte
	{
	  ch = (k[pos] >> 20 ) & 0xff;
	  if ( ch >= 0 && ch < 144 )
	    {

	      p[index+0] = ch;
	      p[index+1] = ((k[pos+2] >>12) & 0xfff);   // time  
	      p[index+2] = ((k[pos] ) & 0xfff);         // highpost
	      p[index+3] = ((k[pos+1] >> 12) & 0xfff);  // lowpost
	      p[index+4] = ((k[pos+1] ) & 0xfff);       // highpre
	      p[index+5] = ((k[pos+2]) & 0xfff);        // lowpre
	      count++;
	      index +=6;
	    }
	  pos+=3;
	}
      else  // look for the right marker by stepping just one
	{
	  pos++;
	}
    }

  return count;
}

