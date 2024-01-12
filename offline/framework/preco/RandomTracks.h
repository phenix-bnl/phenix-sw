#ifndef __RANDOMTRACKS_H__
#define __RANDOMTRACKS_H__

#include "SubsysReco.h"
#include <string>
#include <vector>

class PHCompositeNode;

// This code shuffles track order within an event.  This is necessary because the 
// reconstruction produces ordered tracks (I know this is true for DCH phi and 
// alpha specifically, but there may be others).
// Ordered tracks can lead to structures in the output of pair produced code.

// By registering this SubsysReco module to Fun4All, before your analysis module,
// your analysis module will use randomly distributed tracks and any such structures
// will be removed.

// SCC 11/11/2005 (with some help from TKH :) )

class RandomTracks: public SubsysReco
{
 public:
  // 3 constructors -- can use with just one track type, with 2 track types, or with many in vector form
  RandomTracks(std::vector<std::string> TrackTypes);
  RandomTracks(const char *ATrack);
  RandomTracks(const char *ATrack1, const char *ATrack2);

  virtual ~RandomTracks() {}

  int process_event(PHCompositeNode *topNode);

  void Verbosity(const int ival) {verbosity = ival;}

 protected:
  std::vector<std::string> TrackTypes;
  int ncalls;
  int verbosity;
  unsigned int nTypes;
};

#endif /* __RANDOMTRACKS_H__ */
