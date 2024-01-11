#ifndef __CRKHITMAPENTRY_H_
#define __CRKHITMAPENTRY_H_

#include <iostream>
#include <PHObject.h>

class CrkHitMapEntry : public PHObject
{
 public:
  CrkHitMapEntry();
  virtual ~CrkHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_id(const short int i) {id = i;};
  void set_pmtid(const short val) {pmtid = val;}
  void set_npe(const float val) {npe = val;}
  void set_time(const float val) {time = val;}
  // Here are the very explicit "get" routines...
  short int get_id() {return id;}
  short int get_pmtid() const {return pmtid;}
  float get_npe() const {return npe;}
  float get_time() const {return time;}

 protected:
  short int id;
  short int pmtid;
  float npe;
  float time;

  ClassDef(CrkHitMapEntry,1)
};

#endif /* CRKHITMAPENTRY */
