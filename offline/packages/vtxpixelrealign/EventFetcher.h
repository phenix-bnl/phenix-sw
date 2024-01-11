#ifndef EventFetcher_h
#define EventFetcher_h

#include <map>
#include <set>
#include <string>

class SingleEventFetcher;
class SvxPixelRawHitList;

class EventFetcher
{

 public:
  EventFetcher() {}
  virtual ~EventFetcher();
  int OpenDST(const std::string &name);
  SvxPixelRawHitList *fetchevent(const int evtseq);

 private:
  std::map<int, SingleEventFetcher *> inputs;
  std::map<int, int> evtseg;
  std::set<std::string> files;
};

#endif
