#ifndef __TRACKPROJECTIONMAPENTRY_H_
#define __TRACKPROJECTIONMAPENTRY_H_

#include <iostream>
#include <map>
#include <vector>

class TrackProjectionMapEntry
{
 public:
  TrackProjectionMapEntry();
  virtual ~TrackProjectionMapEntry() {}

  void identify(std::ostream &os=std::cout) const;

  float get_projection(const short int det_id, const short int i) const;
  void AddProjection(const short int det_id, float fvec[]);
  void Reset();
  short int getSize();
  float *getMap();
 protected:
  float alldets_array[20][3];
};

#endif /* TRACKPROJECTIONMAPENTRY */

