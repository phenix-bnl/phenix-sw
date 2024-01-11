#ifndef __ACCHITMAP_H
#define __ACCHITMAP_H

#include "AccHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class AccHitMap : public PHObject
{
 public:

  AccHitMap();
  virtual ~AccHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const AccHitMapEntry &hit);
  virtual const AccHitMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, AccHitMapEntry> acchitmap;

  ClassDef(AccHitMap,1)
};

#endif /* __ACCHITMAP_H */
