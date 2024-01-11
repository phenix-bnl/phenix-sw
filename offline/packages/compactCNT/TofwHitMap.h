#ifndef __TOFWHITMAP_H
#define __TOFWHITMAP_H

#include "TofwHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TofwHitMap : public PHObject
{
 public:

  TofwHitMap(){}
  virtual ~TofwHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const TofwHitMapEntry &hit);
  virtual const TofwHitMapEntry *GetHit(const short int index) const;


 protected:

  std::map<short int, TofwHitMapEntry> tofwhitmap;

};

#endif /* __TOFWHITMAP_H */
