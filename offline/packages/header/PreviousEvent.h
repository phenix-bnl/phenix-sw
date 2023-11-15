#ifndef __PREVIOUSEVENT_H
#define __PREVIOUSEVENT_H

#include <iostream>

#include "phool.h"
#include "PHObject.h"

///
class PreviousEvent: public PHObject
{
 public:

  /// dtor
  virtual ~PreviousEvent() {}

  /// Clear Event
  virtual void Reset();
  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;
  /// isValid returns non zero if object contains valid data
  virtual int isValid() const;
  virtual int get_clockticks(const int /*i*/) const {return -9999;}
  virtual void set_clockticks(const int /*ival*/, const int /*i*/) {return;}

 private: // prevent doc++ from showing ClassDef
  ClassDef(PreviousEvent,1)

};

#endif



