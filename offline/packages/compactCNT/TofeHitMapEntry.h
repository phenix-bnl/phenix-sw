#ifndef __TOFEHITMAPENTRY_H_
#define __TOFEHITMAPENTRY_H_

#include <iostream>

class TofeHitMapEntry
{
 public:
  TofeHitMapEntry();
  virtual ~TofeHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_xyz(const int i, const float val) {xyz[i] = val;}
  void set_slatid(const short val) {slatid = val;}
  void set_id(const short int i) {id = i;};
  void set_etof(const float val) {etof = val;}
  void set_ttof(const float val) {ttof = val;}
  // Here are the very explicit "get" routines...
  float get_xyz (const int i) const { return xyz[i];}
  float get_etof() const {return etof;}
  float get_ttof() const {return ttof;}
  short int get_slatid() const {return slatid;}
  short int get_id() {return id;}

 protected:
  short int id;
  short int slatid;
  float xyz[3];
  float etof;
  float ttof;
};

#endif /* TOFEHITMAPENTRY */
