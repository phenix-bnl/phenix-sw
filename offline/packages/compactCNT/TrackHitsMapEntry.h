#ifndef __TRACKHITSMAPENTRY_H_
#define __TRACKHITSMAPENTRY_H_

#include <iostream>
#include <map>

class TrackHitsMapEntry
{
 public:
  TrackHitsMapEntry();
  virtual ~TrackHitsMapEntry() {}

  void identify(std::ostream &os=std::cout) const;

  short int get_hitid(const short int det_id) const;
  void AddHit(const short int det_id, const short int hitid);
  void Reset();
  short int *getMap() {return &alldets_array[0];}
  short int getSize();

 protected:
  short int alldets_array[20];
};

#endif /* TRACKHITSMAPENTRY */

