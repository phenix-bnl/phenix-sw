#include <PHSnglTrack.h>

#include <cmath>

ClassImp(PHSnglTrack)

using namespace std;

static int shutup = 0;

float
PHSnglTrack::get_projectionVtx(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionDch(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPc1(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPc2(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPc3(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionSvx(const short ilayer, const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionCrk(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionTec(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionTof(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPbSc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPbGl(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionAcc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionTzr(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionPcr(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionTofw(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionMrpc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_projectionHbd(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionVtx(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionDch(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPc1(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPc2(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPc3(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionSvx(const short ilayer, const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionCrk(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionTec(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionTof(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPbSc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPbGl(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionAcc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionTzr(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionPcr(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionMrpc(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionTofw(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_directionHbd(const short d0) const
{
  return -999999.9;
}

float
PHSnglTrack::get_crkPathLength() const
{
  return -999999.9;
}

float
PHSnglTrack::get_tofPathLength() const
{
  return -999999.9;
}

float
PHSnglTrack::get_emcPathLength() const
{
  return -999999.9;
}

float
PHSnglTrack::get_mrpcPathLength() const
{
  return -999999.9;
}

float
PHSnglTrack::get_tofwPathLength() const
{
  return -999999.9;
}

float
PHSnglTrack::get_tzrPathLength() const
{
  return -999999.9;
}

void
PHSnglTrack::ShutUp(const int i)
{
  shutup = i;
  return ;
}
void
PHSnglTrack::Reset()
{
  PHSnglTrack dummy;
  Copy(dummy);
}

void
PHSnglTrack::Copy(const PHSnglTrack &src)
{
  ShutUp(1);
  set_trackIndex(src.get_trackIndex());
  for (short int i = 0;i < 3;i++)
    {
      set_projectionAcc(i, src.get_projectionAcc(i));
      set_projectionCrk(i, src.get_projectionCrk(i));
      set_projectionDch(i, src.get_projectionDch(i));
      set_projectionTofw(i, src.get_projectionTofw(i));
      set_projectionMrpc(i, src.get_projectionMrpc(i));
      set_projectionPbGl(i, src.get_projectionPbGl(i));
      set_projectionPbSc(i, src.get_projectionPbSc(i));
      set_projectionPc1(i, src.get_projectionPc1(i));
      set_projectionPc2(i, src.get_projectionPc2(i));
      set_projectionPc3(i, src.get_projectionPc3(i));
      set_projectionPcr(i, src.get_projectionPcr(i));
      set_projectionTec(i, src.get_projectionTec(i));
      set_projectionTof(i, src.get_projectionTof(i));
      set_projectionTzr(i, src.get_projectionTzr(i));
      set_projectionVtx(i, src.get_projectionVtx(i));

      set_directionAcc(i, src.get_directionAcc(i));
      set_directionCrk(i, src.get_directionCrk(i));
      set_directionDch(i, src.get_directionDch(i));
      set_directionTofw(i, src.get_directionTofw(i));
      set_directionMrpc(i, src.get_directionMrpc(i));
      set_directionPbGl(i, src.get_directionPbGl(i));
      set_directionPbSc(i, src.get_directionPbSc(i));
      set_directionPc1(i, src.get_directionPc1(i));
      set_directionPc2(i, src.get_directionPc2(i));
      set_directionPc3(i, src.get_directionPc3(i));
      set_directionPcr(i, src.get_directionPcr(i));
      set_directionTec(i, src.get_directionTec(i));
      set_directionTof(i, src.get_directionTof(i));
      set_directionTzr(i, src.get_directionTzr(i));
      set_directionVtx(i, src.get_directionVtx(i));

      for (int j = 0;j < 4;j++)
        {
          set_projectionSvx(j, i, src.get_projectionSvx(j, i));
          set_directionSvx(j, i, src.get_directionSvx(j, i));
        }
    }

  set_emcPathLength(src.get_emcPathLength());
  set_tofwPathLength(src.get_tofwPathLength());
  set_mrpcPathLength(src.get_mrpcPathLength());
  set_crkPathLength(src.get_crkPathLength());
  set_tofPathLength(src.get_tofPathLength());
  set_tzrPathLength(src.get_tzrPathLength());

  ShutUp(0);
  return ;
}
