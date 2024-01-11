#include "TecPacketList.hh"
#include "TecPacket.hh"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecPacketList)

using namespace std;

TecPacketList::TecPacketList() {
  TecPackets = new TClonesArray("TecPacket",1);
}

TecPacketList::~TecPacketList() {
  Clear();
}

void TecPacketList::Clear(Option_t *option) {
  TecPackets->Clear();
  if (TecPackets->GetSize() > 1)
    {
      TecPackets->Expand(1);
    }
}

void TecPacketList::Reset() {
  Clear();
}

int TecPacketList::AddTecPacket(TecPacket* packet) const {

  if(TecPackets->GetEntries()==TecPackets->GetSize()) {
    TecPackets->Expand(TecPackets->GetSize()+TECPACKETLISTEXPANDSIZE);
  }
  TClonesArray &array = *TecPackets;
  new(array[TecPackets->GetEntries()]) TecPacket(packet);

  return TecPackets->GetEntries();

}

//=============================================================================

unsigned int TecPacketList::getPacketID(int i) {
  TecPacket* packet = (TecPacket*)GetTecPackets()->UncheckedAt(i);
  return packet->getPacketID();
}

unsigned int TecPacketList::getScheme(int i) {
  TecPacket* packet = (TecPacket*)GetTecPackets()->UncheckedAt(i);
  return packet->getScheme();
}

unsigned int TecPacketList::getNWords(int i) {
  TecPacket* packet = (TecPacket*)GetTecPackets()->UncheckedAt(i);
  return packet->getNWords();
}

unsigned int* TecPacketList::getFEM(int i) {
  TecPacket* packet = (TecPacket*)GetTecPackets()->UncheckedAt(i);
  return packet->getFEM();
}

unsigned int TecPacketList::getFEM(int i, int j) {
  TecPacket* packet = (TecPacket*)GetTecPackets()->UncheckedAt(i);
  return packet->getFEM(j);
}

void TecPacketList::setPacketID(int i, unsigned int a) {
  ((TecPacket*)GetTecPackets()->UncheckedAt(i))->setPacketID(a);
}

void TecPacketList::setScheme(int i, unsigned int a) {
  ((TecPacket*)GetTecPackets()->UncheckedAt(i))->setScheme(a);
}

void TecPacketList::setNWords(int i, unsigned int a) {
  ((TecPacket*)GetTecPackets()->UncheckedAt(i))->setNWords(a);
}

void TecPacketList::setFEM(int i, unsigned int array[TECPACKETLENGTH]) {
  ((TecPacket*)GetTecPackets()->UncheckedAt(i))->setFEM(array);
}

void TecPacketList::setFEM(int i, int j, unsigned int a) {
  ((TecPacket*)GetTecPackets()->UncheckedAt(i))->setFEM(j,a);
}

void TecPacketList::identify(ostream& out) const {
  out << "I am a TecPacketList object." << endl;
}

int TecPacketList::getNPackets() const
{
return TecPackets->GetEntries();
}

