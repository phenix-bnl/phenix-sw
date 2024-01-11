#ifndef __PADHITMAP_H
#define __PADHITMAP_H

#include "PadHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class PadHitMap : public PHObject
{
 public:

  PadHitMap(){}
  virtual ~PadHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const PadHitMapEntry &hit);
  virtual const PadHitMapEntry *GetHit(const short int index) const;


 protected:

  std::map<short int, PadHitMapEntry> padhitmap;

  ClassDef(PadHitMap,1)
};

#endif /* __PADHITMAP_H */
