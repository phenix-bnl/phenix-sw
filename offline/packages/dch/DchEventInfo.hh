#ifndef __DCHEVENTINFO_H__
#define __DCHEVENTINFO_H__

class DchEventInfo {
public:
  DchEventInfo() {}
  virtual ~DchEventInfo() {}

  int   getEvent() { return event;}
  void setEvent( int val) { event = val;}

private:
  int event;
};

#endif /*__DCHEVENTINFO_H__ */



