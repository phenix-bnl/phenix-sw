//
// This is the raw data object.  It is called with the readData(*packet)
// routine which fills the very simple arrays.
// There are a couple gets and sets for the raw tdc/adc info.
//
// SCJ (17 January 2002) First iteration.

#include <iostream>

#include "Event.h"
#include "FclCompactRawv1.h"

ClassImp(FclCompactRawv1)

using namespace std;

FclCompactRawv1::FclCompactRawv1()
{
  Reset();
  return;
}

void FclCompactRawv1::Reset()
{

  for (int channel = 0; channel < CHANTOT; channel++)
    {
      setTdc(channel, ADCZERO);
      setLowAdcPost(channel, ADCZERO);
      setLowAdcPre(channel, ADCZERO);
    }
  whichSide = -1;
  return ;
}

void FclCompactRawv1::identify(std::ostream& out) const
{
  out << "dst version of the fcl raw object, FclCompactRawv1" << endl;
}

int FclCompactRawv1::isValid() const
{
  if(whichSide==-1)
{
 return 0;
}
  return 1;
}

void FclCompactRawv1::readData(Packet *packet, int iSide){

  whichSide = iSide;
  for (int channel = 0; channel < CHANTOT; channel++){
    // In case of zero suppression the TDC value will be exactly zero.  
    // In this case, set it to an unreasonably large number so it will not
    // satisfy the criteria of a real hit.
    if (packet->iValue(channel,0)==0)
      {
      setTdc(channel, 5000);
      }
    else
      {
      setTdc(channel, packet->iValue(channel,0));
      }
    setLowAdcPost(channel, packet->iValue(channel,2));
    setLowAdcPre(channel, packet->iValue(channel,4));
  }

}

int FclCompactRawv1::chanInBounds(int channel){
  if (channel<CHANTOT && channel>=0) 
{
return 1;
}
  else return 0;
}

int FclCompactRawv1::getLowGain(int channel){

  int gain = 0;

  if (!chanInBounds(channel)){
    gain = ADCZERO;
  }
  else{
    gain = getLowAdcPost(channel) - getLowAdcPre(channel) ;
  }
  
  return gain;
}
