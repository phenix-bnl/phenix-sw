#ifndef TRACKLINEPROJECTIONMAP_H
#define TRACKLINEPROJECTIONMAP_H

#include "TrackLineProjectionMapEntry.h"
#include <PHObject.h>
#include <iostream>
#include <map>

class TrackLineProjectionMap : public PHObject
{
 public:
  TrackLineProjectionMap(){}

  virtual ~TrackLineProjectionMap(){}

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Routines to manipulate the particle array
  void AddProjection(const short int index, const TrackLineProjectionMapEntry &projection);
  TrackLineProjectionMapEntry *GetTrackLine(const int index);
  int Entries() const {return tracklineprojectionmap.size();}

 protected:
  std::map<short int,TrackLineProjectionMapEntry> tracklineprojectionmap;

};

#endif /* TRACKLINEPROJECTIONMAP_H */
