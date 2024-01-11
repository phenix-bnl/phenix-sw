
#include "SvxTrackList.h"

ClassImp(SvxTrackList)


//_____________________________________________________________________
unsigned int SvxTrackList::get_n_tracks() const
{
  std::cout << "SvxTrackList::get_n_tracks not overridden" << std::endl;
  return 0;
}

//_____________________________________________________________________
unsigned int SvxTrackList::size() const
{
  std::cout << "SvxTrackList::size() not overridden" << std::endl;
  return 0;
}

//_____________________________________________________________________
void SvxTrackList::insert_track(const SvxTrack*)
{
  std::cout << "SvxTrackList::insert_track() not reimplemented" << std::endl;
  return;
}

//_____________________________________________________________________
SvxTrack* SvxTrackList::get_new_track()
{
  std::cout << "SvxTrackList::get_new_track() not reimplemented" << std::endl;
  return 0;
}

//_____________________________________________________________________
void SvxTrackList::remove_track(unsigned int itrk)
{
  std::cout << "SvxTrackList::remove_track() not reimplemented" << std::endl;
  return;
}

//_____________________________________________________________________
void SvxTrackList::remove_track(SvxTrack* trk)
{
  std::cout << "SvxTrackList::remove_track() not reimplemented" << std::endl;
  return;
}

//_____________________________________________________________________
SvxTrack* SvxTrackList::get_track(unsigned int itrk) const 
{
  std::cout << "SvxTrackList::get_track() not reimplemented" << std::endl;
  return 0;
}

//_____________________________________________________________________
void SvxTrackList::load_associations(SvxClusterList*)
{
  std::cout << "SvxTrackList::load_associations() not reimplemented" << std::endl;
  return;
}

//_____________________________________________________________________
void SvxTrackList::Reset()
{
  std::cout << "SvxTrackList::Reset() not reimplemented" << std::endl;
  return;
}

//_____________________________________________________________________
void SvxTrackList::identify(std::ostream &os) const
{
  os << "identify yourself: virtual SvxTrackList object" << std::endl;
  return;
}
