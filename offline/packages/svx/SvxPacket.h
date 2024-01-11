#ifndef SVXPACKET_H
#define SVXPACKET_H 

#include "TObject.h"
#include <iostream>
#include "SvxParameters.h"
#include <string.h>

/**
 Author: Sasha Lebedev <lebedev@iastate.edu> 
 Represents one PIXEL VTX packet (or module in hardware language). 
 There are 60 VTX pixel packets, each packet corresponds
 to a half-ladder (two sensors).
 Each packet contain 8 readout chips, each chip contains 256
 32-bit rows (individual pixels). 
 Packet ID go from 24000 to 24059 
 Based on online_distribution/newbasic/Packet_pxl_dcm0 class.
*/

class SvxPacket : public TObject {
 
 public:
 
  SvxPacket(); 
  SvxPacket(const SvxPacket* packet);
    virtual ~SvxPacket() { }

  void identify(std::ostream& os = std::cout) const;

  int *getArray()  {return _packet;}
  int getArray(int i)  {
   if(i>-1 && i<SVXPACKETLENGTH) {return _packet[i];}
     else {return -1;}
  }

  void setArray(int array[SVXPACKETLENGTH]) {
    for(int i=0; i<SVXPACKETLENGTH; i++) _packet[i]=array[i];
  }

  void setArray(int i, int a) {
    _packet[i]=a;
  }

  int    iValue(const int channel, const char *what);
  int    iValue(const int chip, const int row);

//////////////////////////////
  void show() {
    for(int i=0; i<SVXPACKETLENGTH; i++) std::cout<<i<<" "<<std::hex<<_packet[i]<<std::dec<<std::endl;
  }

  

//////////////////////////////

protected:

  int _packet[SVXPACKETLENGTH];

  int *decode (int *);
  int *decode_misc (int *);

  ClassDef(SvxPacket,1)

};

#endif                                                          

