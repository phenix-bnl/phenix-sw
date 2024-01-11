#ifndef VTXPIXELREALIGNRECO_H__
#define VTXPIXELREALIGNRECO_H__

#include <SubsysReco.h>

#ifndef __CINT__
#include <boost/version.hpp> // to get BOOST_VERSION
#if (__GNUC__ == 4 && __GNUC_MINOR__ == 8 && \
    (BOOST_VERSION == 106300 || BOOST_VERSION == 105700))
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#include <boost/numeric/interval.hpp>
#pragma GCC diagnostic warning "-Wunused-local-typedefs"
#else
#include <boost/numeric/interval.hpp>
#endif
#endif

#include <map>
#include <string>
#include <sstream>
#include <vector>

class EventFetcher;
class SvxPixelRawHitList;

class VtxPixelRealignReco: public SubsysReco
{
 public:
  VtxPixelRealignReco(const std::string &name = "VTXPIXELREALIGN");
  virtual ~VtxPixelRealignReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  int OpenDST(const std::string &name);
  void PrintMap() const;
  void insertmap(const int eventno, const int chipno, const int jump);
  void ReadFile(const std::string &fname);
  void DropEvents(const int nevnt) {dropevents=nevnt;}
  void UseDB(const int i=1) {usedb = i;}
  int fetch(const int run);
  void print() const;

 protected:
  int runnumber;
  int dropevents;
  int usedb;
  EventFetcher *evfetch;
  SvxPixelRawHitList *svxpixelhits[4]; // [0] is current event
  std::map<int, std::map<int,int> > eventchipmap;
  std::map<int,int> chipmap;
#ifndef __CINT__
  std::map<int, boost::numeric::interval<int> > dropeventrange; 
#endif
};

#endif /* VTXPIXELREALIGNRECO_H__ */
