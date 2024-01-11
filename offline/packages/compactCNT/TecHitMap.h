#ifndef __TECHITMAP_H
#define __TECHITMAP_H

#include "TecHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TecHitMap : public PHObject
{
 public:

  TecHitMap();
  virtual ~TecHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const TecHitMapEntry &hit);
  virtual const TecHitMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, TecHitMapEntry> techitmap;

};

#endif /* __TECHITMAP_H */
