#ifndef __TRACKLINEPROJECTIONMAPENTRY_H_
#define __TRACKLINEPROJECTIONMAPENTRY_H_

#include <iostream>
#include <map>
#include <vector>

class TrackLineProjectionMapEntry
{
 public:
  TrackLineProjectionMapEntry(){}
  virtual ~TrackLineProjectionMapEntry() {}

  void identify(std::ostream &os=std::cout) const;

  float get_projection(const short int det_id, const short int i) const;
  void AddProjection(const short int det_id, std::vector<float> &fvec);
  void Reset();
  std::map<short int, std::vector<float> > *getMap() {return &alldets;}
 protected:
  std::map<short int, std::vector<float> > alldets;

};

#endif /* TRACKPROJECTIONMAPENTRY */

