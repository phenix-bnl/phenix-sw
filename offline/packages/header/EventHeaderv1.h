#ifndef __EVENTHEADERv1_H
#define __EVENTHEADERv1_H

#include <iostream>
#include <ctime>

#include "EventHeader.h"

///
/*
   The EventHeaderDummy class is necessary since I screwed up in the
   Implementation of the virtual base class (no entry in the LinkDef file)
   In order to be able to read old dst's but keep the virtual EventHeader
   interface for the analysis code we need to put an (to root) unknown class
   between EventHeader and EventHeaderv1. That does not affect the compiled
   code but satisfies the weird ways in which root is working right now
*/
class EventHeaderDummy: public EventHeader
{
 public:
  /// dtor
  virtual ~EventHeaderDummy() {}
};

class EventHeaderv1: public EventHeader
{
 public:

  /// ctor
  EventHeaderv1();
  /// dtor
  virtual ~EventHeaderv1() {}

  EventHeaderv1 * clone() const { return new EventHeaderv1(*this); }

  ///  Clear Event
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  int isValid() const;

  /// get Event Number
  int get_EvtSequence() const {return EvtSequence;}
  /// set Event Number
  void set_EvtSequence(const int evtno) {EvtSequence=evtno; return;}

  /// get Event Type (Data,rejected,EOR,BOR,...)
  int get_EvtType() const {return EvtType;}
  /// set Event Type (Data,rejected,EOR,BOR,...)
  void set_EvtType(const int ival) {EvtType = ival; return;}

  /// get ATP TimeStamp (unix time, convert with ctime() to date string 
  time_t  get_TimeStamp() const {return TimeStamp;}
  /// set ATP TimeStamp
  void set_TimeStamp(const time_t evttime) {TimeStamp = evttime; return;}

 protected:

  int EvtSequence;  // Event number
  int EvtType;      // Data type (Data,Rejected,Scaler,PPG ...)
  time_t TimeStamp;  // TimeStamp of Evt from ATP in Ticks 

 private: // prevent doc++ from showing ClassDef
  ClassDef(EventHeaderv1,1)
};

#endif
