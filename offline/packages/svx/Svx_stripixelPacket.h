#ifndef SVXSTRIPPACKET_H
#define SVXSTRIPPACKET_H 

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

class Svx_stripixelPacket : public TObject {
 
 public:
 
  
  Svx_stripixelPacket();
  Svx_stripixelPacket(const Svx_stripixelPacket* packet);
  virtual ~Svx_stripixelPacket() { }

  void identify(std::ostream& os = std::cout) const;

  int *getArray()  {return _packet;}
  int getArray(int i)  {
   if(i>-1 && i<SVXSTRIPPACKETLENGTH) {return _packet[i];}
     else {return -1;}
  }

  void setArray(int array[SVXSTRIPPACKETLENGTH]) {
    for(int i=0; i<SVXSTRIPPACKETLENGTH; i++) _packet[i]=array[i];
  }

  void setArray(int i, int a) {
    _packet[i]=a;
  }

  

//////////////////////////////
  

  void show_stripixel(){
     for(int j=0; j<SVXSTRIPPACKETLENGTH; j++) std::cout<<j<<" "<<std::hex<<_packet[j]<<std::dec<<std::endl;
  }

//////////////////////////////

protected:

  int _packet[SVXSTRIPPACKETLENGTH];

  

  ClassDef(Svx_stripixelPacket,1)

};

#endif                                                          

