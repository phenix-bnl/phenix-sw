#ifndef __FCLCOMPACTRAWV1__
#define __FCLCOMPACTRAWV1__

#include <iostream>

#include "FclConsts.h"
#include "FclRaw.h"

class Packet;

class FclCompactRawv1: public FclRaw
{

 public:

  FclCompactRawv1(); // Default constructor
  void readData(Packet *packet, int iSide); // Write the data into the arrays.

  void Reset();
  void identify(std::ostream& out = std::cout) const;
  int isValid() const;

  // GETS:
  int getTdc(int channel) {
    if (chanInBounds(channel)) return tdc[channel];
    else return -1;}
  int getLowAdcPost(int channel) {
    if (chanInBounds(channel)) return lowAdcPost[channel];
    else return -1;}
  int getLowAdcPre(int channel) {
    if (chanInBounds(channel)) return lowAdcPre[channel];
    else return -1;}
  int getLowGain(int);

  int getSide(){return whichSide;}
  
  //SETS:
  int setTdc(int channel, int value){
    if (chanInBounds(channel)){ tdc[channel]=value; return 1;}
    else return -1;}
  int setLowAdcPost(int channel, int value){
    if (chanInBounds(channel)){ lowAdcPost[channel]=value; return 1;}
    else return -1;}
  int setLowAdcPre(int channel, int value){
    if (chanInBounds(channel)){ lowAdcPre[channel]=value; return 1;}
    else return -1;}

 protected:

  int chanInBounds(int channel);

  int whichSide;
  
  int tdc[CHANTOT];
  int lowAdcPost[CHANTOT];
  int lowAdcPre[CHANTOT];

  ClassDef(FclCompactRawv1,1)
};


#endif 

