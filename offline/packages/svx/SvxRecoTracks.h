#ifndef __SVXRECOTRACKS_H_
#define __SVXRECOTRACKS_H_

#include "SvxRecoSingleTrack.h"
#include <vector>

class SvxRecoTracks
{
  public:
    SvxRecoTracks()
    {
      vertex[0]=0.;
      vertex[1]=0.;
      vertex[2]=0.;
    }
    ~SvxRecoTracks(){}
    
    SvxRecoSingleTrack* getTrack(int track);
    
    void addTrack(const SvxRecoSingleTrack &trk);
    
    int getNTracks();
    
    void reset();
    
    float getVertex(int coor);
    
    
  private:
    
    std::vector<SvxRecoSingleTrack> tracks;
    float vertex[3];
    
    
};


inline SvxRecoSingleTrack* SvxRecoTracks::getTrack(int track)
{
  return &(tracks[track]);
}

inline void SvxRecoTracks::addTrack(const SvxRecoSingleTrack &trk)
{
  tracks.push_back(trk);
}

inline int SvxRecoTracks::getNTracks(){return tracks.size();}

inline void SvxRecoTracks::reset(){tracks.clear();}

inline float SvxRecoTracks::getVertex(int coor){return vertex[coor];}









#endif
