///////////////////////////////////////////////////////////////////////////
//
//  packet_lvl2decision.h
//
//    Header file defining the interface to the lvl2 decision history
//      reader object.
//
//  $Log: packet_lvl2decision.h,v $
//  Revision 1.7  2010/09/21 19:37:50  phoncs
//  DLW: change name of DWORD to PHDWORD
//
//  Revision 1.6  2001/12/22 19:12:37  purschke
//  mlp -- inadvertantly changes the lvl packet. Set it back.
//
//  Revision 1.4  2001/12/09 23:26:28  chiu
//  added getLvl1AlgorithmDecision()
//
//  Revision 1.3  2001/09/04 08:46:02  phoncs
//  jfrantz --added a method to assist in Lvl2Decision monitoring
//
//  Revision 1.2  2001/08/27 13:56:17  phnxlvl1
//  major revisions to remove explicit dependence on Lvl2Decision.h
//
//  Revision 1.1  2001/08/23 16:00:15  cole
//  First versions
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef __PACKET_LVL2DECISION_H__
#define __PACKET_LVL2DECISION_H__

#include <packet_w124.h>


//  The level-2 primitive data are written in long (32-bit) words so
//    we inherit from Packet_w4
//
#ifndef __CINT__
class WINDOWSEXPORT Packet_lvl2decision : public Packet_w4 {
#else
class  Packet_lvl2decision : public Packet_w4 {
#endif

public:
  enum {
    MaxNumLvl1Triggers = 32,
    MaxNumAlgorithms = 64
  };

  enum {
    FullDecision, NumLevel1Triggers, Level1TriggerDecision, AlgorithmDecision
  };

  typedef UINT AlgorithmDecisionArray[MaxNumAlgorithms];
  typedef UINT Lvl1DecisionArray[MaxNumLvl1Triggers];

private:

  bool _unpacked;

  UINT _finalDecision;
  UINT _numLvl1Triggers;

  AlgorithmDecisionArray _algorithmDecision;
  Lvl1DecisionArray _lvl1Decision;
  AlgorithmDecisionArray _lvl1AlgorithmDecision[MaxNumLvl1Triggers];

  void unpack();

public:


  Packet_lvl2decision(PACKET_ptr data);
  
  ~Packet_lvl2decision() {}

  int  iValue(const int channel, const char *what);
  int  iValue(const int channel, const int what);

  const UINT getFinalDecisionMask() 
  { 
    if (!_unpacked) unpack();
    return _finalDecision; 
  }

  const AlgorithmDecisionArray& getAlgorithmDecisionArray()
  {
    if (!_unpacked) unpack();
    return _algorithmDecision;
  }

  const Lvl1DecisionArray& getDecisionArray() 
  {
    if (!_unpacked) unpack();
    return _lvl1Decision;
  }
  
  const AlgorithmDecisionArray& getLvl1AlgorithmDecisionArray(UINT lvl1trig) 
  {
    if (!_unpacked) unpack();
    return _lvl1AlgorithmDecision[lvl1trig];
  }

  UINT getLvl1AlgorithmDecision(UINT lvl1trig, UINT lvl2alg)
  {
    if (!_unpacked) unpack();
    return _lvl1AlgorithmDecision[lvl1trig][lvl2alg];
  }

  void dump ( OSTREAM& );

  UINT getDecisionLength() const
  {
    return getPacketDataLength(packet);
  }

  PHDWORD* getDecisionPtr() const {return findPacketDataStart(packet);}

protected:

  virtual int *decode (int *);
};

#endif /* __PACKET_LVL2DECISION_H__ */

















