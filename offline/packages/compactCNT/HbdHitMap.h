#ifndef __HBDHITMAP_H
#define __HBDHITMAP_H

#include "HbdHitMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class HbdHitMap : public PHObject
{
 public:

  HbdHitMap();
  virtual ~HbdHitMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const HbdHitMapEntry &hit);
  virtual const HbdHitMapEntry *get_cell(const short int index) const;

  int  get_nCells() const;

 protected:

  int Nentries;
  // From Takao Sakaguchi, the maximum number of HBD pads is 2304
  HbdHitMapEntry hbdhitmap[2304];

  ClassDef(HbdHitMap,1)
};

#endif /* __HBDHITMAP_H */
