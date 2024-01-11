#ifndef __TRACKHITSMAP_H
#define __TRACKHITSMAP_H

#include "TrackHitsMapEntry.h"
#include <PHObject.h>

#include <iostream>
#include <map>

class TrackHitsMap : public PHObject
{
 public:
  TrackHitsMap(){}

  virtual ~TrackHitsMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddHits(const short int index, const TrackHitsMapEntry &hits);
  TrackHitsMapEntry *GetTrack(const int index);

 protected:
  std::map<short int,TrackHitsMapEntry> trackhitsmap;
};

#endif /* __TRACKHITSMAP_H */
