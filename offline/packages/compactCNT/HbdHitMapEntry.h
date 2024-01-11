#ifndef __HBDHITMAPENTRY_H_
#define __HBDHITMAPENTRY_H_

#include <PHObject.h>

#include <iostream>

class HbdHitMapEntry : public PHObject
{
 public:
  HbdHitMapEntry();
  virtual ~HbdHitMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_adcch(const short int ival) {adcch = ival;}
  void set_charge(const short int ival) {charge = ival;}

  // Here are the very explicit "get" routines...
  short int get_adcch() const {return adcch;}
  short int get_charge() const {return charge;}

 protected:
  short int adcch;
  short int charge;

  ClassDef(HbdHitMapEntry,1)
};

#endif /* HBDHITMAPENTRY */
