#include <PHTrackOut.h>

#include <TClonesArray.h>

ClassImp(PHTrackOut)

using namespace std;

static int shutup = 0;

PHSnglTrack *
PHTrackOut::get_track(const unsigned int ntrk) const
{
  if (!GetPHTrk())
    {
      return 0;
    }
  if ((int)ntrk > GetPHTrk()->GetLast())
    {
      cerr << PHWHERE << " Track Index out of range, size of track TC Array: "
	   << GetPHTrk()->GetLast() << ", requested index: " << ntrk
	   << endl;
      return 0;
    }
  PHSnglTrack *phtrk = (PHSnglTrack *) GetPHTrk()->UncheckedAt(ntrk);
  return phtrk;
}

TClonesArray *
PHTrackOut::GetPHTrk() const
{
  cerr << "Using Base class GetPHTrk()" << endl;
  return 0;
}

int 
PHTrackOut::set_TClonesArraySize(const unsigned int ntrk)
{
  if (!GetPHTrk())
    {
      return 0;
    }
// set TC array to correct size and call default ctor for all sngl objects
  GetPHTrk()->ExpandCreate(ntrk); 
  return ntrk;
}

void PHTrackOut::Reset()
{
  cerr << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int PHTrackOut::isValid() const
{
  cerr << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void PHTrackOut::identify(ostream &os) const
{
  os << "identify yourself: virtual PHTrackOut object" << endl;
  return ;
}

void
PHTrackOut::ShutUp(const int i)
{
  shutup = i;
}

void
PHTrackOut::virtual_warning(const char *funcname) const
{
  if (!shutup)
    {
       cerr << "PHTrackOut::" << funcname << " is virtual, doing nothing" << endl;
    }
  return ;
}
