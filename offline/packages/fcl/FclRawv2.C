
//
// This is the raw data object.  It is called with the readData(*packet)
// routine which fills the very simple arrays.
// There are a couple gets and sets for the raw tdc/adc info.
//
// SCJ (17 January 2002) First iteration.
// JN (25 January 2006) V2 of FclRaw does not include the run-by-run
// calibration info in every event.

#include "Event.h"
#include "FclRawv2.h"

ClassImp(FclRawv2)

using namespace std;

FclRawv2::FclRawv2(){
  Reset();
}

void FclRawv2::Reset(){

  FclCompactRawv1::Reset();

  for (int channel = 0; channel < CHANTOT; channel++){

    setHighAdcPost(channel,ADCZERO);
    setHighAdcPre(channel,ADCZERO);

  }
}

void FclRawv2::identify(ostream& out) const
{
  out<<"Full fcl raw object FclRawv2"<<endl;
  return;
}

int FclRawv2::isValid() const
{
  if(whichSide==-1) return 0;
  return 1;
}

void FclRawv2::readData(Packet *packet, int iSide){

  FclCompactRawv1::readData(packet,iSide);

  whichSide = iSide;
  for (int channel = 0; channel < CHANTOT; channel++){

    setHighAdcPost(channel, packet->iValue(channel,1));
    setHighAdcPre(channel, packet->iValue(channel,3));
  }

}

int FclRawv2::getHighGain(int channel){

  int gain = 0;

  if (!chanInBounds(channel)) {
    gain = ADCZERO;
  }
  else{
    gain = getHighAdcPost(channel) - getHighAdcPre(channel) ;
  }
  
  return gain;
}

int FclRawv2::getCalibration(){
  
  // Calibration has been moved to FclCalib object

  return 0;
  
}

float FclRawv2::getHighGainCalib(int channel){
  // Calibrated data moved to FclOut
  return -1.0;
}

float FclRawv2::getLowGainCalib(int channel){
  // Calibrated data moved to FclOut
  return -1.0;
}
