#ifndef __SVXTRACKMAP_H
#define __SVXTRACKMAP_H

#include "SvxTrackMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class SvxTrackMap : public PHObject
{
 public:

  SvxTrackMap();
  virtual ~SvxTrackMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHit(const short int index, const SvxTrackMapEntry &hit);
  virtual const SvxTrackMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, SvxTrackMapEntry> svxtrackmap;

 ClassDef(SvxTrackMap, 1);

};

#endif 
