#ifndef __TRACKPATHLENGTHMAP_H
#define __TRACKPATHLENGTHMAP_H

#include "TrackPathLengthMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TrackPathLengthMap : public PHObject
{
 public:
  TrackPathLengthMap(){}

  virtual ~TrackPathLengthMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddPathLength(const short int index, const TrackPathLengthMapEntry &pathlength);
  TrackPathLengthMapEntry *GetTrack(const int index);

 protected:
  std::map<short int,TrackPathLengthMapEntry> trackpathlengthmap;
};

#endif /* __TRACKPATHLENGTHMAP_H */
