#ifndef EVENTLOCATE_H
#define EVENTLOCATE_H

#include <PHObject.h>

#include <iostream>
#include <map>

class EventLocate: public PHObject
{
 public:
  EventLocate() {}
  virtual ~EventLocate() {}

  virtual void Reset();
  virtual void identify(std::ostream& os = std::cout) const;
  virtual int isValid() const {return 0;}
  virtual void insert(const int evtseq, const int evtcount) {return;}
  virtual int get_entry( const int evtseq) const {return 0xFFFFFFF;}
  virtual std::pair<std::map<int, int>::const_iterator, std::map<int, int>::const_iterator> beginend();

 protected:
   ClassDef(EventLocate,1)

};

#endif /* __EVENTLOCATE_H */

