
//
// This is the raw data object.  It is called with the readData(*packet)
// routine which fills the very simple arrays.
// There are a couple gets and sets for the raw tdc/adc info.
//
// SCJ (17 January 2002) First iteration.

#include "Event.h"
#include "FclRawv1.h"

ClassImp(FclRawv1)

using namespace std;

FclRawv1::FclRawv1(){
  gotCalibration = 0;
  Reset();
}

void FclRawv1::Reset(){

  FclCompactRawv1::Reset();

  for (int channel = 0; channel < CHANTOT; channel++){

    setHighAdcPost(channel,ADCZERO);
    setHighAdcPre(channel,ADCZERO);

  }
}

void FclRawv1::identify(ostream& out) const
{
  out<<"Full fcl raw object FclRawv1"<<endl;
  return;
}

int FclRawv1::isValid() const
{
  if(whichSide==-1) return 0;
  return 1;
}

void FclRawv1::readData(Packet *packet, int iSide){

  FclCompactRawv1::readData(packet,iSide);

  whichSide = iSide;
  for (int channel = 0; channel < CHANTOT; channel++){

    setHighAdcPost(channel, packet->iValue(channel,1));
    setHighAdcPre(channel, packet->iValue(channel,3));
  }

}

int FclRawv1::getHighGain(int channel){

  int gain = 0;

  if (!chanInBounds(channel)) {
    gain = ADCZERO;
  }
  else{
    gain = getHighAdcPost(channel) - getHighAdcPre(channel) ;
  }
  
  return gain;
}

int FclRawv1::getCalibration(){
  
  // This will be a database access at the end of the week
  // But while I'm trying to figure out what to save I'll
  // simply use an ascii file.
  
  FILE *fp;
  fp = fopen("cosmics.dat","r");
  
  int side, chan, row, col;
  float mean, norm;
  
  for (int iSide=0; iSide<2; iSide++){
    for (int iChan=0; iChan<CHANTOT; iChan++){
      fscanf(fp,"%d %d %d %d %f %f", &side, &chan, &row, &col, &mean, &norm);
      
      if (iSide == whichSide)
	calibrationNorm[iChan] = norm;
      
    }
  }
  
  fclose(fp);

  gotCalibration = 1;

  return 0;
  
}

float FclRawv1::getHighGainCalib(int channel){

  float gain = 0;

  if (getHighAdcPost(channel) < POSTTHRESHOLD ||
      !chanInBounds(channel)) {
    gain = ADCZERO;
  }
  else{
    gain = (float)(getHighAdcPost(channel) - getHighAdcPre(channel))*calibrationNorm[channel] ;
  }
  
  return gain;
}
float FclRawv1::getLowGainCalib(int channel){

  float gain = 0;

  if (getHighAdcPost(channel) < POSTTHRESHOLD || 
      !chanInBounds(channel)){
    gain = ADCZERO;
  }
  else{
    gain = (float)(getLowAdcPost(channel) - getLowAdcPre(channel))*calibrationNorm[channel] ;
  }
  
  return gain;
}
