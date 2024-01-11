#ifndef TECPACKET_H
#define TECPACKET_H 

#include "TObject.h"
#include "TecBasicObject.hh"
#include <iostream>

/** Represents one Tec packet (FEM) */
class TecPacket : public TObject {
 
 public:
 
/// Default constructor
  TecPacket(); 
/// Constructors
  TecPacket(unsigned int packetid, unsigned int scheme, unsigned int nwords, unsigned int array[TECPACKETLENGTH]);
  TecPacket(const TecPacket* packet);

/// Destructor
  virtual ~TecPacket() { }

///
 void identify(std::ostream& os = std::cout) const;

///
  unsigned int getPacketID()  {return PacketID;}
///
  unsigned int getScheme()  {return Scheme;}
///
  unsigned int getNWords()  {return NWords;}
///
  unsigned int *getFEM()  {return FEM;}
///
  unsigned int getFEM(int i)  {
   if(i>-1 && i<TECPACKETLENGTH) {return FEM[i];}
     else {return 0;}
  }

///
  void setPacketID(unsigned int i)  {PacketID=i;}
///
  void setScheme(unsigned int i)  {Scheme=i;}
///
  void setNWords(unsigned int i)  {NWords=i;}
///
  void setFEM(unsigned int array[TECPACKETLENGTH]) {
    for(int i=0; i<TECPACKETLENGTH; i++) FEM[i]=array[i];
  }
///
  void setFEM(int i, unsigned int a) {
    FEM[i]=a;
  }

protected:

///
  unsigned int PacketID;
///
  unsigned int Scheme;
///
  unsigned int NWords;
///
  unsigned int FEM[TECPACKETLENGTH];

  ClassDef(TecPacket,1)

};

#endif                                                          

