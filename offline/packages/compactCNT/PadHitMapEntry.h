#ifndef __PADHITMAPENTRY_H_
#define __PADHITMAPENTRY_H_

#include <PHObject.h>
#include <iostream>

class PadHitMapEntry
{
 public:
  PadHitMapEntry();
  virtual ~PadHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_xyz(const int i, const float val) {xyz[i] = val;}
  void set_id(const short int i) {id = i;};
  // Here are the very explicit "get" routines...
  float get_xyz (const int i) const { return xyz[i];}
  short int get_id() {return id;}
 protected:
  short int id;
  float xyz[3];

  ClassDef(PadHitMapEntry,1)
};

#endif /* PADHITMAPENTRY */
