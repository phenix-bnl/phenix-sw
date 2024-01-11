#ifndef __TOFEHITMAP_H
#define __TOFEHITMAP_H

#include "TofeHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TofeHitMap : public PHObject
{
 public:

  TofeHitMap(){}
  virtual ~TofeHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const TofeHitMapEntry &hit);
  virtual const TofeHitMapEntry *GetHit(const short int index) const;


 protected:

  std::map<short int, TofeHitMapEntry> tofehitmap;

};

#endif /* __TOFEHITMAP_H */
