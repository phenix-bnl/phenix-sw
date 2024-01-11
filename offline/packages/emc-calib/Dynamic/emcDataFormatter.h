#ifndef __EMCDATAFORMATTER_H__
#define __EMCDATAFORMATTER_H__

class Event;
class emcRawDataObject;

//*************************************************************************

class emcDataFormatter
{
public:
  enum limits{HG_MIN = 1024, HG_MAX  = 4095, LG_MIN = 0, LG_MAX = 4095, TAC_MIN = 0, TAC_MAX = 4095};
  enum errorFlags {HG_PRE_OUT  = 0x4, HG_POST_OUT = 0x8, LG_PRE_OUT = 0x40, LG_POST_OUT = 0x80, TAC_OUT = 0x400, CHANNEL_DISABLED = 0x2000};

  emcDataFormatter() {}
  ~emcDataFormatter() {}

  bool fillRDO(Event* ev);

  void empty(emcRawDataObject* rdo, const int iSM);
};

#endif // _emcDataFormatter_
