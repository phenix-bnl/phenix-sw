#ifndef __FCLRAWV2__
#define __FCLRAWV2__

#include <iostream>
#include "FclConsts.h"
#include "FclCompactRawv1.h"

class FclRawv2: public FclCompactRawv1
{

 public:

  FclRawv2(); // Default constructor
  void readData(Packet *packet, int iSide); // Write the data into the arrays.

  void Reset();
  void identify(std::ostream& out = std::cout) const;
  int isValid() const;

  // GETS:
  int getHighAdcPost(int channel) {
    if (chanInBounds(channel)) return highAdcPost[channel];
    else return -1;}
  int getHighAdcPre(int channel) {
    if (chanInBounds(channel)) return highAdcPre[channel];
    else return -1;}
  int getHighGain(int channel);
  float getHighGainCalib(int channel);
  float getLowGainCalib(int channel);
  

  //SETS:
  int setHighAdcPost(int channel, int value){
    if (chanInBounds(channel)){ highAdcPost[channel]=value; return 1;}
    else return -1;}
  int setHighAdcPre(int channel, int value){
    if (chanInBounds(channel)){ highAdcPre[channel]=value; return 1;}
    else return -1;}

  int getCalibration();

  
 private:
  
  int highAdcPost[CHANTOT];
  int highAdcPre[CHANTOT];

  ClassDef(FclRawv2,1)

};


#endif 
