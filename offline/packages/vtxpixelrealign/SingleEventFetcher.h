#ifndef SingleEventFetcher_h
#define SingleEventFetcher_h

#include <iostream>
#include <string>

class EventLocate;
class Fun4AllInputManager;
class PHCompositeNode;
class SvxPixelRawHitList;

class SingleEventFetcher
{
 public:
  SingleEventFetcher();
  virtual ~SingleEventFetcher();

  int FileOpen(const std::string &name);

  void RunNumber(const int runno) {runnumber = runno;}
  void Segment(const int iseg) {segment = iseg;}
  int Segment() const {return segment;}
  void TopNode(PHCompositeNode *top) {topnode = top;}
  EventLocate *GetEventLocate() {return loc;}
  void identify(std::ostream& os = std::cout) const;
  SvxPixelRawHitList *getpixelhits(const int evtseq);
  void Verbosity(const int i) {verbosity = i;}

 protected:
  Fun4AllInputManager *dstin;
  PHCompositeNode *topnode;
  EventLocate *loc;
  int runnumber;
  int segment;
  int verbosity;
  std::string fname;
  std::string topnodename;
  std::string evtlocnodename;
};

#endif
