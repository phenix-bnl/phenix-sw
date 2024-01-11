#include <iostream>
#include "Svx_stripixelPacket.h"

ClassImp(Svx_stripixelPacket)

Svx_stripixelPacket::Svx_stripixelPacket() { 
  for(int i=0; i<18; i++) _packet[i]=0;
  for(int i=18; i<18+4644; i++) _packet[i]=0;
  for(int i=18+4644; i<SVXSTRIPPACKETLENGTH; i++) _packet[i]=0;
}



Svx_stripixelPacket::Svx_stripixelPacket(const Svx_stripixelPacket* a) {
  for(int i=0; i<SVXSTRIPPACKETLENGTH; i++) _packet[i] = a->_packet[i];
}



void Svx_stripixelPacket::identify(std::ostream& out) const {
  out << "I am a Svx Strippixel Packet" << std::endl;
}

