
//-------------------------------------------------------------------
// SvxTrackList.h
// Author: Matt Wysocki, July 22, 2011
//
/// \class SvxTrackList
/// Container base class for SvxTracks
///
/// This is the base class for the container of SvxTracks.  The best
/// way to put a new track in the array is to use get_new_track(),
/// which creates the track directly in the array and returns a
/// pointer to it.
//-------------------------------------------------------------------

#ifndef __SVXTRACKLIST_H__
#define __SVXTRACKLIST_H__

#include <phool.h>
#include <PHObject.h>
#include <iostream>

class SvxTrack;
class SvxClusterList;

class SvxTrackList : public PHObject
{
 public:
  SvxTrackList() {}
  virtual ~SvxTrackList() {}

  //! Number of tracks in the array
  virtual unsigned int get_n_tracks() const;

  //! Number of tracks in the array
  virtual unsigned int size() const;

  //! Get track by array index 
  virtual SvxTrack* get_track(unsigned int itrk) const;

  //! Put a new track object in the array and return it's pointer
  virtual SvxTrack* get_new_track();

  //! Copy an existing track object onto the back of the array
  virtual void insert_track(const SvxTrack*);

  //! Remove a track from the array by index
  virtual void remove_track(unsigned int itrk);

  //! Remove a track from the array using the track pointer
  virtual void remove_track(SvxTrack* trk);

  //! Fill the associated cluster pointers in the track objects
  virtual void load_associations(SvxClusterList*);

  virtual void Reset();
  virtual void identify(std::ostream &os=std::cout) const;

 private:
  ClassDef(SvxTrackList,1)
};
#endif // __SVXTRACKLIST_H__
