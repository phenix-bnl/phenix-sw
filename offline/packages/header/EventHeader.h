#ifndef EVENTHEADER_H
#define EVENTHEADER_H

#include <PHObject.h>
#include <phool.h>

#include <ctime>
#include <iostream>
#include <set>

///
class EventHeader: public PHObject
{
 public:

  /// dtor
  virtual ~EventHeader() {}

  /// Clear Event
  virtual void Reset()
    {
      std::cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << std::endl;
      return;
    }

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const
    {
      os << "identify yourself: virtual EventHeader Object" << std::endl;
      return;
    }

  /// isValid returns non zero if object contains valid data
  virtual int isValid() const
    {
      std::cout << PHWHERE << "isValid not implemented by daughter class" << std::endl;
      return 0;
    }

  /// get Event Number
  virtual int get_EvtSequence() const {return -9999;}
  /// set Event Number
  virtual void set_EvtSequence(const int /*ival*/) {return;}

  /// get Event Type (Data,rejected,EOR,BOR,...)
  virtual int get_EvtType() const {return -9999;}
  /// set Event Type (Data,rejected,EOR,BOR,...)
  virtual void set_EvtType(const int /*ival*/) {return;}

  /// get ATP TimeStamp (unix time, convert with ctime()
  virtual time_t get_TimeStamp() const {return 0;}
  /// set TimeStamp
  virtual void set_TimeStamp(const time_t /*evttime*/) {return;}

  virtual void AddBadPacket(const unsigned int /*ibad*/) {return;}
  virtual int isBadPacket(const unsigned int /*ibad*/) const {return 0;}
  virtual const std::set<unsigned int> *GetBadPacketSet() const {return NULL;}
  virtual int GetPacketInfo(const unsigned int id, std::set<unsigned int> &returnset) const {return 0;}

 private: // prevent doc++ from showing ClassDef
  ClassDef(EventHeader,1)

};

#endif



