#ifndef VTXPIXELREALIGNRECAL_H__
#define VTXPIXELREALIGNRECAL_H__

#include "Recalibrator.h"

#ifndef __CINT__

#if (__GNUC__ == 4 && __GNUC_MINOR__ == 8)
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#pragma message "ignoring bogus gcc warning in boost header boost/numeric/interval.hpp"
#include <boost/numeric/interval.hpp>
#pragma GCC diagnostic warning "-Wunused-local-typedefs"
#else
#include <boost/numeric/interval.hpp>
#endif
#endif

#include <map>
#include <string>

class VtxPixelRealignRecal: public Recalibrator
{
 public:
  VtxPixelRealignRecal();
  virtual ~VtxPixelRealignRecal() {}

  void DropEvents(const int nevnt) {dropevents=nevnt;}
  void ReadFile(const std::string &fname);
  void insertmap(const int eventno, const int chipno, const int jump);
  void print() const;
  int Commit();
  int fetch(const int run);
  int readbacktest();
  void ResetForNextFile();
  void set_runnumber(const int i) {runnumber=i;}

 private:
  std::map<int, std::map<int,int> > eventchipmap;
  std::map<int,int> chipmap;

  int runnumber;
  int dropevents;
  std::string tablename;

#ifndef __CINT__
  std::map<int, boost::numeric::interval<int> > dropeventrange; 
#endif
};

#endif /* VTXPIXELREALIGNRECAL_H__ */
