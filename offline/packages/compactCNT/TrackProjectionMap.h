#ifndef __TRACKPROJECTIONMAP_H
#define __TRACKPROJECTIONMAP_H

#include "TrackProjectionMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TrackProjectionMap : public PHObject
{
 public:
  TrackProjectionMap(){}

  virtual ~TrackProjectionMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddProjection(const short int index, const TrackProjectionMapEntry &projection);
  TrackProjectionMapEntry *GetTrack(const int index);
  int Entries() const {return trackprojectionmap.size();}

 protected:
  std::map<short int,TrackProjectionMapEntry> trackprojectionmap;
};

#endif /* __TRACKPROJECTIONMAP_H */
