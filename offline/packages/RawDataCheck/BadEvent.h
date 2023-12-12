#ifndef __BADEVENT_H__
#define __BADEVENT_H__

#include <string>
#include <iostream>

class BadEvent
{

 public:
  BadEvent();
  virtual ~BadEvent() {}
  //  void Dump(const int nrows = 0) const;
  void identify(std::ostream &os = std::cout) const;
  void Run(const int i) {runno = i;}
  int Run() const {return runno;}
  void Event(const int i) {eventno = i;}
  int Event() const {return eventno;}
  void SegmentId(const int ival) {segmentid = ival;}
  int SegmentId() const {return segmentid;}
  void TrigRaw(const unsigned int ival) {trigraw = ival;}
  unsigned int TrigRaw() const {return trigraw;}
  void TrigLive(const unsigned int ival) {triglive = ival;}
  unsigned int TrigLive() const {return triglive;}
  void TrigScaled(const unsigned int ival) {trigscaled = ival;}
  unsigned int TrigScaled() const {return trigscaled;}
  void Crossing(const int ival) {crossing = ival;}
  int Crossing() const {return crossing;}
  void Reason(const std::string &str) {reason = str;}
  std::string Reason() const {return reason;}
  void CvsTag(const std::string &str) {cvstag = str;}
  std::string CvsTag() const {return cvstag;}
  void TicksToLastEvent(const unsigned int ival) { tickstolastevent = ival;}
  unsigned int TicksToLastEvent() const {return tickstolastevent;}

 private:
  int runno;
  int eventno;
  int segmentid; 
  unsigned int trigraw;
  unsigned int triglive;
  unsigned int trigscaled;
  int crossing;
  std::string reason;
  std::string cvstag;
  unsigned int tickstolastevent;
};

#endif
