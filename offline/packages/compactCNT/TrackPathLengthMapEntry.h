#ifndef __TRACKPATHLENGTHMAPENTRY_H_
#define __TRACKPATHLENGTHMAPENTRY_H_

#include <iostream>
#include <map>
#include <vector>

class TrackPathLengthMapEntry
{
 public:
  TrackPathLengthMapEntry(){}
  virtual ~TrackPathLengthMapEntry() {}

  void identify(std::ostream &os=std::cout) const;

  float get_pathlength(const short int det_id) const;
  void AddPathLength(const short int det_id, const float pl);
  void Reset();
  std::map<short int, float > *getMap() {return &alldets;}

 protected:
  std::map<short int, float > alldets;

};

#endif /* TRACKPATHLENGTHMAPENTRY */

