#ifndef SVXPACKETLIST_h
#define SVXPACKETLIST_h

#include "PHObject.h"
#include "SvxPacket.h"
#include <iostream>

class TClonesArray;

const int SVXPACKETLISTEXPANDSIZE = 1;

/**
A list of VTX Pixel packets.
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
*/
class SvxPacketList : public PHObject {

public:

  SvxPacketList();
  virtual ~SvxPacketList();

  void Reset();
  void Clear(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const;

  SvxPacket *getPacket(int i);

  void setPacket(int i, int array[SVXPACKETLENGTH]);

  int AddSvxPacket(SvxPacket* packet) const;
  TClonesArray *GetSvxPackets() const {return SvxPackets;}
  int getNPackets() const;

protected:

/// List of VTX Pixel Data Packets
  TClonesArray *SvxPackets;

  ClassDef(SvxPacketList,1)

};

#endif


