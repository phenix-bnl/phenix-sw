#ifndef __SVXCENTRALTRACKMAP_H
#define __SVXCENTRALTRACKMAP_H

#include "SvxCentralTrackMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class SvxCentralTrackMap : public PHObject
{
 public:

  SvxCentralTrackMap();
  virtual ~SvxCentralTrackMap(){}

  void Reset();
  int  isValid() const {return 0;}
  void identify(std::ostream &os=std::cout) const;

  void AddHit(const short int index, const SvxCentralTrackMapEntry &hit);
  virtual const SvxCentralTrackMapEntry *GetHit(const short int index) const;

  int GetNentries();

 protected:
  int Nentries;
  std::map<short int, SvxCentralTrackMapEntry> svxtrackmap;

 ClassDef(SvxCentralTrackMap, 1);

};

#endif 
