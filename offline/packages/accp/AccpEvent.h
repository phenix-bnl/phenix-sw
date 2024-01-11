#ifndef __ACCPEVENT_H__
#define __ACCPEVENT_H__

#include "phool.h"
#include "TObject.h"
#include "AccpRaw.h"

class Event;
class PHCompositeNode;

class AccpEvent : public TObject
{
public:
  AccpEvent() : accpraw(0) {}
  virtual ~AccpEvent() {}

  int event(PHCompositeNode *topNode); 
  PHBoolean setRawData (Event * event);
  int print();

private:
  AccpRaw *accpraw;

  ClassDef(AccpEvent,0)
};

#endif
