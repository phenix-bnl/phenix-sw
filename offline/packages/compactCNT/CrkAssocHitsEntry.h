#ifndef __CRKASSOCHITSENTRY_H_
#define __CRKASSOCHITSENTRY_H_

#include <iostream>
#include <PHObject.h>

class CrkAssocHitsEntry : public PHObject
{
 public:
  CrkAssocHitsEntry();
  virtual ~CrkAssocHitsEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // NOTE: The number of Crk hits that can be associated with a track is limited to MAXPMTHITS in CrkPID
  // So has to be limited here to no more than that. The value in CrkPID is set to 20.

  static const int HITSMAX = 20;

  // Here are the very explicit set routines...
  void set_trackid(const int i) {trackid = i;}
  void set_swapped(const bool i) {swapped = i;}
  void set_npmts(const int i) {if(i<HITSMAX) npmts = i; else npmts = HITSMAX;}
  void set_pmtid(const int ihit, const short int val) {if(ihit<HITSMAX) pmtid[ihit] = val;}
  void set_npe(const int ihit, const float val) {if(ihit<HITSMAX) npe[ihit] = val;}
  void set_time(const int ihit, const float val) {if(ihit<HITSMAX) time[ihit] = val;}
  void set_rpmt(const int ihit, const float val) {if(ihit<HITSMAX) rpmt[ihit] = val;}

  // Here are the very explicit "get" routines...
  short int get_trackid() const {return trackid;}
  bool get_swapped() const {return swapped;}
  short int get_npmts() const {return npmts;}
  short int get_pmtid(const int ihit) const {if(ihit<HITSMAX) return pmtid[ihit]; else return -9999;}
  float get_npe(const int ihit) const {if(ihit<HITSMAX) return npe[ihit]; else return -9999.0;}
  float get_time(const int ihit) const {if(ihit<HITSMAX) return time[ihit]; else return -9999.0;}
  float get_rpmt(const int ihit) const {if(ihit<HITSMAX) return rpmt[ihit]; else return -9999.0;}

 protected:

  int trackid;
  bool swapped;
  int npmts;

  short int pmtid[HITSMAX];
  float npe[HITSMAX];
  float time[HITSMAX];
  float rpmt[HITSMAX];

  ClassDef(CrkAssocHitsEntry,1)

};

#endif /* CRKASSOCHITSENTRY */
