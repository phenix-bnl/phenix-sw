#include <iostream>
#include "TecPacket.hh"

ClassImp(TecPacket)

TecPacket::TecPacket() { 
  PacketID = 0;
  Scheme = 0;
  NWords = 0;
  for(int i=0; i<TECPACKETLENGTH; i++) FEM[i]=0;
}

TecPacket::TecPacket(const TecPacket* packet) {
  PacketID = packet->PacketID;
  Scheme = packet->Scheme;
  NWords = packet->NWords;
  for(int i=0; i<TECPACKETLENGTH; i++) FEM[i] = packet->FEM[i];
}

TecPacket::TecPacket(unsigned int packetid, unsigned int scheme, 
           unsigned int nwords, unsigned int array[TECPACKETLENGTH]) {
  PacketID = packetid;
  Scheme = scheme;
  NWords = nwords;
  for(int i=0; i<TECPACKETLENGTH; i++) FEM[i]=array[i];
}

void TecPacket::identify(std::ostream& out) const {
  out << "I am a Tec Packet (FEM)" << std::endl;
}


