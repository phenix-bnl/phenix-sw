#include "SvxPacketList.h"
#include "SvxPacket.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(SvxPacketList)

using namespace std;

SvxPacketList::SvxPacketList() {
  SvxPackets = new TClonesArray("SvxPacket",1);
}

SvxPacketList::~SvxPacketList() {
  Clear();
}

void SvxPacketList::Clear(Option_t *option) {
  SvxPackets->Clear();
//  if (SvxPackets->GetSize() > 1)
//    {
//      SvxPackets->Expand(1);
//    }
}

void SvxPacketList::Reset() {
  Clear();
}

int SvxPacketList::AddSvxPacket(SvxPacket* packet) const {

  if(SvxPackets->GetEntries()==SvxPackets->GetSize()) {
    SvxPackets->Expand(SvxPackets->GetSize()+SVXPACKETLISTEXPANDSIZE);
  }
  TClonesArray &array = *SvxPackets;
  new(array[SvxPackets->GetEntries()]) SvxPacket(packet);

  return SvxPackets->GetEntries();

}

//=============================================================================

SvxPacket* SvxPacketList::getPacket(int i) {
  SvxPacket* packet = (SvxPacket*)GetSvxPackets()->UncheckedAt(i);
  return packet;
}

void SvxPacketList::setPacket(int i, int array[SVXPACKETLENGTH]) {
  ((SvxPacket*)GetSvxPackets()->UncheckedAt(i))->setArray(array);
}

void SvxPacketList::identify(ostream& out) const {
  out << "I am a SvxPacketList object." << endl;
}

int SvxPacketList::getNPackets() const
{
return SvxPackets->GetEntries();
}


