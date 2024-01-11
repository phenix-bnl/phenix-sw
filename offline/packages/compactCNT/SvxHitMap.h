#ifndef __SVXHITMAP_H
#define __SVXHITMAP_H

#include "SvxHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class SvxHitMap : public PHObject
{
 public:

  SvxHitMap();
  virtual ~SvxHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const SvxHitMapEntry &hit);
  virtual const SvxHitMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, SvxHitMapEntry> svxhitmap;


 ClassDef(SvxHitMap, 1);
};

#endif 
