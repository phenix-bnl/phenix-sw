#ifndef __FCLOUTV1__
#define __FCLOUTV1__

#include "FclConsts.h"
#include "FclOut.h"

class Packet;
class FclCalib;

class FclOutv1: public FclOut
{
 public:

  FclOutv1();

  void readData(Packet *packet, int iSide);
  void readData(FclRaw *data);
  void calibrateData(FclCalib &calib);
  void Reset();
  void identify(std::ostream& out = std::cout) const;
  int isValid() const;
  void print() const;

  // GETS:
  int getSide() { return whichSide; }
  float getLowGain(int channel) {
    if (chanInBounds(channel)) return gain[channel];
    else return FCL_INVALID_FLOAT;}

  float getLowGain(int row, int col);

  float getSumAll() {return sumAll;}
  float getSumGrey() {return sumGrey;}

  //SETS

  int setLowGain(int channel, float value){
    if (chanInBounds(channel)){ gain[channel]=value; return 1;}
    else return -1;}

  int setSumAll(float value){ sumAll=value;return 1;}
  int setSumGrey(float value){ sumGrey=value;return 1;} 

  int computeSums();

 protected:
  
  int chanInBounds(int channel);

  int whichSide;
  float gain[CHANTOT];
  float sumAll;
  float sumGrey;

  ClassDef(FclOutv1,1)
    };

#endif
