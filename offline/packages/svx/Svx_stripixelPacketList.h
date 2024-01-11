#ifndef SVXSTRIPPACKETLIST_h
#define SVXSTRIPPACKETLIST_h

#include "PHObject.h"
#include "Svx_stripixelPacket.h"
#include <iostream>

class TClonesArray;

const int SVXSTRIPPACKETLISTEXPANDSIZE = 1;

/**
A list of VTX Pixel packets.
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
*/
class Svx_stripixelPacketList : public PHObject {

public:

  Svx_stripixelPacketList();
  virtual ~Svx_stripixelPacketList();

  void Reset();
  void Clear(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const;

  Svx_stripixelPacket *getPacket(int i);

  void setPacket(int i, int array_stripixel[SVXSTRIPPACKETLENGTH]);

  int AddSvx_stripixelPacket(Svx_stripixelPacket* packet) const;
  TClonesArray *GetSvx_stripixelPackets() const {return Svx_stripixelPackets;}
  int getNPackets() const;

protected:

/// List of VTX strippixel Data Packets
  TClonesArray *Svx_stripixelPackets;

  ClassDef(Svx_stripixelPacketList,1)

};

#endif


