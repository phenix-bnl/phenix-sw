#include "MpcExRawHitv1.h"

#include <vector>

using namespace std;

MpcExRawHitv1::~MpcExRawHitv1()
{
  Reset();
  return;
}

void
MpcExRawHitv1::Reset()
{
  mpcexrawhits.clear();
  return;
}

unsigned int
MpcExRawHitv1::fillfromvector(const vector<unsigned int> &vec)
{
  mpcexrawhits = vec;
  return mpcexrawhits.size();
}

unsigned int
MpcExRawHitv1::gethit(const unsigned int index) const
{
  if (index < mpcexrawhits.size())
    {
      return mpcexrawhits[index];
    }
  cout << "index: " << index << " outside range 0 - " << mpcexrawhits.size() << endl;
  return 0x0;
}

unsigned int
MpcExRawHitv1::getid(const unsigned int index) const
{
  if (index < mpcexrawhits.size())
    {
      return (mpcexrawhits[index] & 0xFFFF0000) >> 16;
    }
  cout << "index: " << index << " outside range 0 - " << mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getadcs(const unsigned int index) const
{
  if (index < mpcexrawhits.size())
    {
      return (mpcexrawhits[index] & 0x0000FFFF);
    }
  cout << "index: " << index << " outside range 0 - " << mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getarm(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
	  //Arm is 0 for South and 1 for North
      return ( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) / (8*4*12*64);
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getpkt(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//Packet ID goes from 0 to 7 individually for North and South
        return ( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) / (4*12*64) - getarm(index)*8;
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getchipmap(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//ROCbond index per packet 0 to 3072
        return ( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) % (4*12*64);
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getchain(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//Chain in packet 0 to 4
        return (( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) % (4*12*64)) / (12*64);
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getchip(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//Chip in chain index per packet 0 to 12
        return ( (( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) % (4*12*64)) - getchain(index)*(12*64) ) / 64;
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getmicromodule(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//Micromodule on chain from 0 to 6
        return ( (( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) % (4*12*64)) - getchain(index)*(12*64) ) / 128;
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getrocbond(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
		//ROCbond on a micromodule 0 to 128
        return (( ( mpcexrawhits[index]& 0xFFFF0000 ) >> 16 ) % (4*12*64)) % 128;
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::gethadc(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
    	return (mpcexrawhits[index] & 0x0000FF00) >> 8;
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}
unsigned int
MpcExRawHitv1::getladc(const unsigned int index) const
{
  if (index <  mpcexrawhits.size())
    {
    	return (mpcexrawhits[index] & 0x000000FF);
    }
  cout << "index: " << index << " outside range 0 - " <<  mpcexrawhits.size() << endl;
  return 0x0;
}

unsigned int
MpcExRawHitv1::getOnlineKey(const unsigned int index) const
{
   return getid(index);
}
