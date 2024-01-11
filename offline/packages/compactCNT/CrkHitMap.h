#ifndef __CRKHITMAP_H
#define __CRKHITMAP_H

#include "CrkHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class CrkHitMap : public PHObject
{
 public:

  CrkHitMap();
  virtual ~CrkHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const CrkHitMapEntry &hit);
  virtual const CrkHitMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, CrkHitMapEntry> crkhitmap;

  ClassDef(CrkHitMap,1)
};

#endif /* __CRKHITMAP_H */
