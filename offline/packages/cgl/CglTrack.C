#include "CglTrack.h"
#include "phool.h"

#include "TClonesArray.h"

#include <iostream>

ClassImp(CglTrack)

using namespace std;

static int shutup = 0;

void CglTrack::Reset()
{
  cerr << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int CglTrack::isValid() const
{
  cerr << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void CglTrack::identify(ostream &os) const
{
  os << "identify yourself: virtual CglTrack object" << endl;
  return ;
}

TClonesArray *
CglTrack::GetCglTrk() const
{
  virtual_warning("GetCglTrk()");
  return 0;
}

void CglTrack::FillFromWrapper(dCglTrackWrapper *wrap)
{
  virtual_warning("FillFromWrapper(dCglTrackWrapper *wrap)");
  return ;
}

void CglTrack::FillFromClass(CglTrack *cgltrack)
{
  virtual_warning("FillFromClass(CglTrack *cgltrack)");
  return ;
}

unsigned int CglTrack::get_CglNTrack() const
{
  virtual_warning("get_CglNTrack()");
  return 0;
}
void CglTrack::set_CglNTrack(const unsigned int ntrk)
{
  virtual_warning("set_CglNTrack(const unsigned int ntrk)");
  return ;
}

int 
CglTrack::set_TClonesArraySize(const unsigned int ntrk)
{
  if (!GetCglTrk())
    {
      return 0;
    }
// set TC array to correct size and call default ctor for all sngl objects
// without knowing what they are
  GetCglTrk()->ExpandCreate(ntrk); 
  return ntrk;
}

CglSnglTrack *
CglTrack::get_track(const unsigned int ntrk) const
{
  if (!GetCglTrk())
    {
      return 0;
    }
  if ((int) ntrk > GetCglTrk()->GetLast())
    {
      cout << PHWHERE << " Track Index out of range, size of track TC Array: "
	   << GetCglTrk()->GetLast() << ", requested index: " << ntrk
	   << endl;
      return 0;
    }
  CglSnglTrack *cgltrk = (CglSnglTrack *) GetCglTrk()->UncheckedAt(ntrk);
  return cgltrk;
}


short CglTrack::get_arm(const unsigned int itrk) const
{
  virtual_warning("get_arm(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_arm(const unsigned int itrk, const short ival)
{
  virtual_warning("set_arm(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_accrecid(const unsigned int itrk) const
{
  virtual_warning("get_accrecid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_accrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_accrecid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_mrpcrecid(const unsigned int itrk) const
{
  virtual_warning("get_mrpcrecid(const unsigned int itrk)");
  return -9999;
}

void CglTrack::set_mrpcrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_mrpcrecid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_tofwrecid(const unsigned int itrk) const
{
  virtual_warning("get_tofwrecid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_tofwrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_tofwrecid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_dctracksid(const unsigned int itrk) const
{
  virtual_warning("get_dctracksid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_dctracksid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_dctracksid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_emcclusid(const unsigned int itrk) const
{
  virtual_warning("get_emcclusid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_emcclusid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_emcclusid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_hbdblobid(const unsigned int itrk) const
{
  virtual_warning("get_hbdblobid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_hbdblobid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_hbdblobid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_id(const unsigned int itrk) const
{
  virtual_warning("get_id(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_id(const unsigned int itrk, const short ival)
{
  virtual_warning("set_id(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_pc1clusid(const unsigned int itrk) const
{
  virtual_warning("get_pc1clusid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_pc1clusid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_pc1clusid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_pc2clusid(const unsigned int itrk) const
{
  virtual_warning("get_pc2clusid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_pc2clusid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_pc2clusid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_pc3clusid(const unsigned int itrk) const
{
  virtual_warning("get_pc3clusid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_pc3clusid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_pc3clusid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_svxclusid(const unsigned int itrk, const short ilayer) const
{
  virtual_warning("get_svxclusid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_svxclusid(const unsigned int itrk, const short ilayer, const short ival)
{
  virtual_warning("set_svxclusid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_pcrrecid(const unsigned int itrk) const
{
  virtual_warning("get_pcrrecid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_pcrrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_pcrrecid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_richringid(const unsigned int itrk) const
{
  virtual_warning("get_richringid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_richringid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_richringid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_tectrackid(const unsigned int itrk) const
{
  virtual_warning("get_tectrackid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_tectrackid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_tectrackid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_tecplaneid(const unsigned int itrk, const unsigned int iplane) const
{
  virtual_warning("get_tecplaneid(const unsigned int itrk, const unsigned int iplane)");
  return -9999;
}
void CglTrack::set_tecplaneid(const unsigned int itrk, const unsigned int iplane, const short ival)
{
  virtual_warning("set_tecplaneid(const unsigned int itrk, const unsigned int iplane, const short ival)");
  return ;
}

short CglTrack::get_tofrecid(const unsigned int itrk) const
{
  virtual_warning("get_tofrecid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_tofrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_tofrecid(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_trackModel(const unsigned int itrk) const
{
  virtual_warning("get_trackModel(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_trackModel(const unsigned int itrk, const short ival)
{
  virtual_warning("set_trackModel(const unsigned int itrk, const short ival)");
  return ;
}

short CglTrack::get_tzrrecid(const unsigned int itrk) const
{
  virtual_warning("get_tzrrecid(const unsigned int itrk)");
  return -9999;
}
void CglTrack::set_tzrrecid(const unsigned int itrk, const short ival)
{
  virtual_warning("set_tzrrecid(const unsigned int itrk, const short ival)");
  return ;
}

float CglTrack::get_quality(const unsigned int itrk) const
{
  virtual_warning("get_quality(const unsigned int itrk)");
  return -9999.9;
}
void CglTrack::set_quality(const unsigned int itrk, const float rval)
{
  virtual_warning("set_quality(const unsigned int itrk, const float rval)");
  return ;
}

void
CglTrack::ShutUp(const int i)
{
  shutup = i;
}

void CglTrack::virtual_warning(const char *funcname) const
{
  if (!shutup)
    {
      cerr << "CglTrack::" << funcname << " is virtual, doing nothing" << endl;
    }
  return ;
}
