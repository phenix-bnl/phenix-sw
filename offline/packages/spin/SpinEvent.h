#ifndef __SPINEVENT_H__
#define __SPINEVENT_H__

#include "phool.h"
#include "TObject.h"
#include "SpinDataEventOut.h"

class Event;
class Packet;
class PHCompositeNode;

class SpinEvent : public TObject
{
  public:
              SpinEvent();
    virtual  ~SpinEvent(){}
    int       event(PHCompositeNode* topNode); 
    PHBoolean setRawData(Event* event);
    void      setGL1PacketData(Packet* pGL1);
    void      setGL1PPacketData(Packet* pGL1P, int iboard);
    void      setGL1PSumPacketData(Packet* pGL1PSum);
    SpinDataEventOut* getSpinDataEventOut();
    int       print();

  private:

    SpinDataEventOut* pSpinDataEventOut;

    int ShiftCrossing;

  ClassDef(SpinEvent,0)
};

#endif
