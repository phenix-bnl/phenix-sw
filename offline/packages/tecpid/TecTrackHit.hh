#ifndef TECTRACKHIT_H
#define TECTRACKHIT_H 

#include "TecTrackV2.hh"
#include "TecHit.hh"

#include<vector>

/** Represents a single Tec track with associated hits */
class TecTrackHit {
 
 public:
 
  TecTrackHit() {}                                                                  
  TecTrackHit(const TecTrackV2& track);                                                                   
  ~TecTrackHit();

  void Clear();

  TecTrackV2* getTrack() {return &TRACK;}
  TecHit* getHit(int i);
  int getNhits() {return (int)HITS.size();}

  //void AddTrack(TecTrackV2& track);
  int AddHit(TecHit& hit);

protected:

  TecTrackV2 TRACK;
  std::vector<TecHit> HITS;
};

#endif /* TECTRACKHIT_H */                                                         

