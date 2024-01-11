#include "Svx_stripixelPacketList.h"
#include "Svx_stripixelPacket.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(Svx_stripixelPacketList)

using namespace std;

Svx_stripixelPacketList::Svx_stripixelPacketList() {
  Svx_stripixelPackets = new TClonesArray("Svx_stripixelPacket",1);
}

Svx_stripixelPacketList::~Svx_stripixelPacketList() {
  Clear();
}

void Svx_stripixelPacketList::Clear(Option_t *option) {
  Svx_stripixelPackets->Clear();
//  if (SvxPackets->GetSize() > 1)
//    {
//      SvxPackets->Expand(1);
//    }
}

void Svx_stripixelPacketList::Reset() {
  Clear();
}

int Svx_stripixelPacketList::AddSvx_stripixelPacket(Svx_stripixelPacket* packet) const {

  if(Svx_stripixelPackets->GetEntries()==Svx_stripixelPackets->GetSize()) {
    Svx_stripixelPackets->Expand(Svx_stripixelPackets->GetSize()+SVXSTRIPPACKETLISTEXPANDSIZE);
  }
  TClonesArray &array_stripixel = *Svx_stripixelPackets;
  new(array_stripixel[Svx_stripixelPackets->GetEntries()]) Svx_stripixelPacket(packet);

  return Svx_stripixelPackets->GetEntries();

}

//=============================================================================

Svx_stripixelPacket* Svx_stripixelPacketList::getPacket(int i) {
  Svx_stripixelPacket* packet = (Svx_stripixelPacket*)GetSvx_stripixelPackets()->UncheckedAt(i);
  return packet;
}

void Svx_stripixelPacketList::setPacket(int i, int array_stripixel[SVXSTRIPPACKETLENGTH]) {
  ((Svx_stripixelPacket*)GetSvx_stripixelPackets()->UncheckedAt(i))->setArray(array_stripixel);
}

void Svx_stripixelPacketList::identify(ostream& out) const {
  out << "I am a Svx_stripixelPacketList object." << endl;
}

int Svx_stripixelPacketList::getNPackets() const
{
return Svx_stripixelPackets->GetEntries();
}


