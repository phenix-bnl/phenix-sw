#ifndef __PREVIOUSEVENTv1_H
#define __PREVIOUSEVENTv1_H

#include <iostream>

#include "PreviousEvent.h"

class PreviousEventv1: public PreviousEvent
{
 public:

  /// ctor
  PreviousEventv1();
  /// dtor
  virtual ~PreviousEventv1() {}

  PreviousEventv1 * clone() const { return new PreviousEventv1(*this); }
  
  ///  Clear Previous
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  int isValid() const;

  int get_clockticks(const int i) const {return clockticks[i];}
  void set_clockticks(const int ival, const int i) {clockticks[i] = ival;}

 protected:

  int clockticks[3];

 private: // prprevious doc++ from showing ClassDef
  ClassDef(PreviousEventv1,1)
};

#endif
