#ifndef __PDBMUTDCMMAP_HH__
#define __PDBMUTDCMMAP_HH__

#include "PdbCalChan.hh"

class PdbMutDCMMap : public PdbCalChan {

public:
  PdbMutDCMMap();
  ~PdbMutDCMMap();

  virtual void print() const;

  int getArm()        const {return arm;} 
  int getStation()    const {return station;}
  int getOctant()     const {return oct;}
  int getHalfOctant() const {return halfOct;}
  int getGap()        const {return gap;}
  int getPlane()      const {return plane;}
  int getDCMChannel(int strip) const {return DCMChannel[strip];}
  int getPacketID(int strip) const {return PacketID[strip];}

  void setAllIdentifiers(int newarm, int newsta, int newoct, int newhalfoct,
			 int newgap, int newplane);
  void setDCMChannel (int strip, int temp){DCMChannel[strip]=temp;}
  void setPacketID   (int strip, int temp){PacketID[strip]=temp;}


private:
  static const int MaxStrips=160;    
  // Max # of strips is determined from Station 3 North

  int arm; 
  int station;
  int oct;
  int halfOct;
  int gap;
  int plane;
  int DCMChannel[MaxStrips];
  int PacketID[MaxStrips];

  ClassDef(PdbMutDCMMap,1);
};

#endif /* __PDBMUTDCMMAP_HH__ */
