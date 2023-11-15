#ifndef __PDBMUTADAM_HH__
#define __PDBMUTADAM_HH__

#include "PdbCalChan.hh"
#include "PHString.h"
#include "PHTimeStamp.h"

class PdbMutADAM : public PdbCalChan {

public:
  PdbMutADAM();
  // PdbMutADAM(const PdbMutADAM& rhs);
  ~PdbMutADAM();
  PdbMutADAM& operator = (const PdbMutADAM &p);

  virtual void print() const;

  int   getIndex        ()  const {return      Index;}
  int   getModule       ()  const {return      Module;}
  int   getSlot         ()  const {return      Slot;}
  int   getChannel      ()  const {return      Channel;}
  float getTemperature  ()  const {return      Temperature;}
  PHTimeStamp getTime   ()        {return      Time;}
  int   getMsec         ()  const {return      Msec;}
  int   getStatus       ()  const {return      Status;}

  void  setIndex        (int         temp)  {Index       = temp;}
  void  setModule       (int         temp)  {Module      = temp;}
  void  setSlot         (int         temp)  {Slot        = temp;}
  void  setChannel      (int         temp)  {Channel     = temp;}
  void  setTemperature  (float       temp)  {Temperature = temp;}
  void  setTime         (PHTimeStamp temp)  {Time        = temp;}
  void  setMsec         (int         temp)  {Msec        = temp;}
  void  setStatus       (int         temp)  {Status      = temp;}

private:
  int   Index;   
  int   Module; //\\PHONCS8\Fastwel.Adamopc.1\SMT# ,1:AI(Magnet Internal Air)
                //                                 ,3:Bit(Light Source)
  int   Slot;   //\\PHONCS8\Fastwel.Adamopc.1\SMT3.Slot#
  int   Channel;//\\PHONCS8\Fastwel.Adamopc.1\SMT3.Slot2_AI_#
  float Temperature;
  PHTimeStamp Time;  
  int   Msec;
  int   Status; // good : 1  bad : 0

  ClassDef(PdbMutADAM,1);
};

#endif /* __PDBMUTADAM_HH__ */
