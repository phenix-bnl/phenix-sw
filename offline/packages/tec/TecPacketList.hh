#ifndef TECPACKETLIST_h
#define TECPACKETLIST_h

#include "PHObject.h"
#include "TecPacket.hh"
#include <iostream>

class TClonesArray;

const int TECPACKETLISTEXPANDSIZE = 1;

/**
A list of Tec packets.
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
*/
class TecPacketList : public PHObject {

public:

///
  TecPacketList();
///
  virtual ~TecPacketList();

///
  void Reset();
///
  void Clear(Option_t *option = "");
///
  void identify(std::ostream& os = std::cout) const;


///
  unsigned int getPacketID(int i);
///
  unsigned int getScheme(int i);
///
  unsigned int getNWords(int i);
///
  unsigned int *getFEM(int i);
///
  unsigned int getFEM(int i, int j);

///
  void setPacketID(int i, unsigned int a);
///
  void setScheme(int i, unsigned int a);
///
  void setNWords(int i, unsigned int a);
///
  void setFEM(int i, unsigned int array[TECPACKETLENGTH]);
///
  void setFEM(int i, int j, unsigned int a);

///
  int AddTecPacket(TecPacket* packet) const;
///
  TClonesArray *GetTecPackets() const {return TecPackets;}
///
  int getNPackets() const;



protected:

/// List of Tec Packets
  TClonesArray *TecPackets;

  ClassDef(TecPacketList,1)

};

#endif


