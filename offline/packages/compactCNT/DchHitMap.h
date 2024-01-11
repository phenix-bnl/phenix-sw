#ifndef __DCHHITMAP_H
#define __DCHHITMAP_H

#include "DchHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class DchHitMap : public PHObject
{
 public:

  DchHitMap(){}
  virtual ~DchHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const DchHitMapEntry &hit);
  virtual const DchHitMapEntry *GetHit(const short int index) const;


 protected:

  std::map<short int, DchHitMapEntry> dchhitmap;

};

#endif /* __DCHHITMAP_H */
