#include <CglSnglTrack.h>

#include <cmath>

ClassImp(CglSnglTrack)

using namespace std;

static int shutup = 0;

float
CglSnglTrack::get_quality() const
{
  return NAN;
}

void
CglSnglTrack::ShutUp(const int i)
{
  shutup = i;
  return ;
}

void
CglSnglTrack::Reset()
{
  CglSnglTrack dummy;
  Copy(dummy);
  return;
}


void
CglSnglTrack::Copy(const CglSnglTrack &src)
{
  ShutUp(1);
  set_arm(src.get_arm());
  set_id(src.get_id());
  set_dctracksid(src.get_dctracksid());
  set_tectrackid(src.get_tectrackid());
  set_pc1clusid(src.get_pc1clusid());
  set_pc2clusid(src.get_pc2clusid());
  set_pc3clusid(src.get_pc3clusid());
  set_tofrecid(src.get_tofrecid());
  set_accrecid(src.get_accrecid());
  set_mrpcrecid(src.get_mrpcrecid());
  set_tofwrecid(src.get_tofwrecid());
  set_hbdblobid(src.get_hbdblobid());
  set_emcclusid(src.get_emcclusid());
  set_richringid(src.get_richringid());
  set_tzrrecid(src.get_tzrrecid());
  set_pcrrecid(src.get_pcrrecid());
  set_trackModel(src.get_trackModel());
  set_quality(src.get_quality());
  for (int i = 0; i < 4;i++)
    {
      set_svxclusid(i, src.get_svxclusid(i));
    }
  ShutUp(0);
  return ;
}
