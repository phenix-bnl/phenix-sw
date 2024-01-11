#ifndef EVENTLOCATEV1_H
#define EVENTLOCATEV1_H

#include "EventLocate.h"

#include <iostream>
#include <map>

class EventLocatev1: public EventLocate
{
 public:
  EventLocatev1();
  virtual ~EventLocatev1() {}

  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;
  void insert(const int evtseq, const int evtcount);
  int get_entry( const int evtseq) const;
  std::pair<std::map<int, int>::const_iterator, std::map<int, int>::const_iterator> beginend() {return std::make_pair(evtloc.begin(), evtloc.end());}
 protected:
  std::map<int, int> evtloc;

   ClassDef(EventLocatev1,1)

};

#endif /* __EVENTLOCATEV1_H */

