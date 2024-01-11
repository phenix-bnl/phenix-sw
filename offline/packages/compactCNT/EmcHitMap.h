#ifndef __EMCHITMAP_H
#define __EMCHITMAP_H

#include "EmcHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class EmcHitMap : public PHObject
{
 public:

  EmcHitMap(){}
  virtual ~EmcHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const EmcHitMapEntry &hit);
  virtual const EmcHitMapEntry *GetHit(const short int index) const;


 protected:

  std::map<short int, EmcHitMapEntry> emchitmap;

};

#endif /* __EMCHITMAP_H */
