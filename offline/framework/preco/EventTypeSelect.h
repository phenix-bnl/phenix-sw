#ifndef __EVENTTYPESELECT_H__
#define __EVENTTYPESELECT_H__


#include "SubsysReco.h"

#include <set>
#include <string>

class EventTypeSelect: public SubsysReco
{
 public:
  EventTypeSelect(const std::string &name = "EVENTTYPESELECT");
  virtual ~EventTypeSelect() {}

  int process_event(PHCompositeNode *topNode);
  void Print(const std::string&) const;

  int AddEventType(const int itype);
  int RemoveEventType(const int itype);
  int SetReturnCode(const char *action = "DISCARD");

 protected:

  std::set <int> eventtypes;
  int RetCode;
};

#endif /* __EVENTTYPESELECT_H__ */
