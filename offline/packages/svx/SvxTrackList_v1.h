
//-------------------------------------------------------------------
// SvxTrackList_v1.h
// Author: Matt Wysocki, July 22, 2011
//
/// \class SvxTrackList_v1
/// Container class for SvxTracks
///
/// This is the v1 class for the container of SvxTracks.  The best
/// way to put a new track in the array is to use get_new_track(),
/// which creates the track directly in the array and returns a
/// pointer to it.
//-------------------------------------------------------------------

#ifndef __SVXTRACKLIST_V1_H__
#define __SVXTRACKLIST_V1_H__

#include <phool.h>
#include <PHObject.h>
#include <TClonesArray.h>
#include <iostream>

#include "SvxTrackList.h"
#include "SvxTrack_v1.h"

class SvxTrackList_v1 : public SvxTrackList
{
 public:
  /// short-cut to current version of the interface object
  /// 
  ///  A trick from Hugo:
  ///  The version of this object can safely be modified with no need to
  ///  change the container version and without breaking
  ///  backward-compatibility, because it is never used as a member of
  ///  the container
  ///
  typedef SvxTrack_v1 ValueImp;

  SvxTrackList_v1();
  ~SvxTrackList_v1();

  // The "standard PHObject response" functions...
  void Reset();
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  unsigned int get_n_tracks() const
  {return size();}

  unsigned int size() const
  {return _track_array->GetEntries();}

  // Get existing track
  SvxTrack* get_track(unsigned int itrk) const;

  // Adding/removing tracks
  SvxTrack* get_new_track();
  void insert_track(const SvxTrack* trk_ptr);
  void remove_track(unsigned int itrk);
  void remove_track(SvxTrack* trk);

  void load_associations(SvxClusterList*);

 protected:
  unsigned int _next_index;

  TObjArray* _track_array;

 private:
  ClassDef(SvxTrackList_v1,1)
};

#endif // __SVXTRACKLIST_V1_H__

