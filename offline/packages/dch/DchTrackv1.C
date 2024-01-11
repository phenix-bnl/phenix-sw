#include "DchTrackv1.h"
#include "DchSnglTrackv1.h"

#include "dDchTracksWrapper.h"
#include "dDchTracksExtWrapper.h"

#include <PHPoint.h>

#include <iostream>

#define DCHTRACKARRAYSIZE  200

ClassImp(DchTrackv1);

using namespace std;

DchTrackv1::DchTrackv1()
{
  DchNTrack = 0;
  DchTrk = new TClonesArray("DchSnglTrackv1", DCHTRACKARRAYSIZE);
  return ;
}

DchTrackv1::~DchTrackv1()
{
  if (DchTrk)
    {
      DchTrk->Clear();
      delete DchTrk;
    }
  return ;
}

void DchTrackv1::identify(ostream& out) const
{
  out << "identify yourself: I am a DchTrackv1 object" << endl;
  return;
}

int DchTrackv1::isValid() const
{
  return((DchNTrack > 0) ? 1 : 0);
}

int DchTrackv1::AddTrack(DchSnglTrackv1 *track, const int itrk)
{
  if (itrk < DchTrk->GetSize())
    {
      TClonesArray &newhit = *DchTrk;
      new (newhit[itrk]) DchSnglTrackv1(track);
      return itrk;
    }
  else
    {
      cout << PHWHERE << "TClonesArray size of "
           << DchTrk->GetSize() << " too small for adding track no: " << endl;
      return -1;
    }
}

void DchTrackv1::Clear(Option_t *option)
{
  DchTrk->Clear();
  if (DchNTrack > DCHTRACKARRAYSIZE)
    {
      DchTrk->Expand(DCHTRACKARRAYSIZE);
    }
  DchNTrack = 0;
  return;
}

void DchTrackv1::Reset()
{
  Clear();
  return;
}

int DchTrackv1::set_TClonesArraySize(const unsigned int fullsize)
{
  if (fullsize > DCHTRACKARRAYSIZE)
    {
      DchTrk->Expand(fullsize);
    }
  return fullsize;
}

DchSnglTrackv1* DchTrackv1::get_Track(const int i) const
{
  if (i < DchTrk->GetSize())
    {
      return (DchSnglTrackv1 *) (DchTrk->UncheckedAt(i));
    }
  else
    {
      cout << PHWHERE << "Dch Track " << i
           << " does not exist, number of tracks: " << DchNTrack << endl;
    }
  return NULL;
}

void DchTrackv1::set_trackid(const unsigned int i, const short val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_trackid(val);
}
void DchTrackv1::set_arm(const unsigned int i, const short val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_arm(val);
}
void DchTrackv1::set_side(const unsigned int i, const short val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_side(val);
}
void DchTrackv1::set_quality(const unsigned int i, const short val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_quality(val);
}
void DchTrackv1::set_phi(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_phi(val);
}
void DchTrackv1::set_alpha(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_alpha(val);
}
void DchTrackv1::set_beta(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_beta(val);
}
void DchTrackv1::set_betaNoVertex(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_betaNoVertex(val);
}
void DchTrackv1::set_zed(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_zed(val);
}
void DchTrackv1::set_phi0(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_phi0(val);
}
void DchTrackv1::set_theta0(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_theta0(val);
}
void DchTrackv1::set_momentum(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_momentum(val);
}

void DchTrackv1::set_status(const unsigned int i, int val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_status(val);
}
void DchTrackv1::set_alpha1(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_alpha1(val);
}
void DchTrackv1::set_alpha2(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_alpha2(val);
}
void DchTrackv1::set_chi21(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_chi21(val);
}
void DchTrackv1::set_chi22(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_chi22(val);
}
void DchTrackv1::set_dist1(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_dist1(val);
}
void DchTrackv1::set_dist2(const unsigned int i, const float val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_dist2(val);
}

void DchTrackv1::set_point(const unsigned int i, const PHPoint val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_point(val);
}

void DchTrackv1::set_direction(const unsigned int i, const PHPoint val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_direction(val);
}

void DchTrackv1::set_hits(const unsigned int i, const short plane, const short val)
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) trk->set_hits(plane, val);
}

short DchTrackv1::get_trackid(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_trackid();
  else return 0;
}

short DchTrackv1::get_arm(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_arm();
  else return 0;
}

short DchTrackv1::get_side(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_side();
  else return 0;
}

short DchTrackv1::get_quality(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_quality();
  else return 0;
}

float DchTrackv1::get_phi(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_phi();
  else return 0;
}

float DchTrackv1::get_alpha(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_alpha();
  else return 0;
}

float DchTrackv1::get_beta(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_beta();
  else return 0;
}

float DchTrackv1::get_betaNoVertex(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_betaNoVertex();
  else return 0;
}

float DchTrackv1::get_zed(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_zed();
  else return 0;
}

float DchTrackv1::get_phi0(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_phi0();
  else return 0;
}
float DchTrackv1::get_theta0(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_theta0();
  else return 0;
}

float DchTrackv1::get_momentum(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_momentum();
  else return 0;
}

PHPoint DchTrackv1::get_point(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_point();
  else
    {
      PHPoint null(-9999, -9999, -9999);
      return null;
    }
}

int    DchTrackv1::get_status(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_status();
  else return 0;
}

float  DchTrackv1::get_alpha1(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_alpha1();
  else return 0;
}

float  DchTrackv1::get_alpha2(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_alpha2();
  else return 0;
}

float  DchTrackv1::get_chi21(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_chi21();
  else return 0;
}

float  DchTrackv1::get_chi22(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_chi22();
  else return 0;
}

float  DchTrackv1::get_dist1(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_dist1();
  else return 0;
}

float  DchTrackv1::get_dist2(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_dist2();
  else return 0;
}

short  DchTrackv1::get_nx1hits(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_nx1hits();
  else return 0;
}

short  DchTrackv1::get_nx2hits(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_nx2hits();
  else return 0;
}

PHVector DchTrackv1::get_direction(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_direction();
  else
    {
      PHVector null(-9999, -9999, -9999);
      return null;
    }
}

short DchTrackv1::get_hits(const unsigned int i, const short plane) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_hits(plane);
  else return 0;
}
short DchTrackv1::get_pc1hit(const unsigned int i) const
{
  DchSnglTrackv1* trk = get_Track(i);
  if (trk) return trk->get_hits(39);
  else return 0;
}

void
DchTrackv1::FillFromWrapper(dDchTracksWrapper *wrap, dDchTracksExtWrapper *wrapext)
{
  if (wrap && wrapext)
    {
      PHPoint pt;
      set_TClonesArraySize(wrap->RowCount());
      for (unsigned int itrk = 0; itrk < wrap->RowCount(); itrk++)
        {
          DchSnglTrackv1* trk = new DchSnglTrackv1();
          trk->set_arm( wrap->get_arm(itrk));
          trk->set_quality( wrap->get_quality(itrk));
          trk->set_side( wrap->get_side(itrk));
          trk->set_trackid( wrap->get_trackid(itrk));
          trk->set_status( wrapext->get_status(itrk));
          trk->set_alpha( wrap->get_alpha(itrk));
          trk->set_alpha1( wrapext->get_alpha1(itrk));
          trk->set_alpha2( wrapext->get_alpha2(itrk));
          trk->set_beta( wrap->get_beta(itrk));
          trk->set_betaNoVertex( wrap->get_betaNoVertex(itrk));
          trk->set_chi21( wrapext->get_chi21(itrk));
          trk->set_chi22( wrapext->get_chi22(itrk));
          trk->set_dist1( wrapext->get_dist1(itrk));
          trk->set_dist2( wrapext->get_dist2(itrk));
          trk->set_momentum( wrap->get_momentum(itrk));
          trk->set_phi( wrap->get_phi(itrk));
          trk->set_phi0( wrap->get_phi0(itrk));
          trk->set_theta0( wrap->get_theta0(itrk));
          trk->set_zed( wrap->get_zed(itrk));

          pt.setX(wrap->get_point(0, itrk));
          pt.setY(wrap->get_point(1, itrk));
          pt.setZ(wrap->get_point(2, itrk));
          trk->set_point(pt);
          pt.setX(wrap->get_direction(0, itrk));
          pt.setY(wrap->get_direction(1, itrk));
          pt.setZ(wrap->get_direction(2, itrk));
          trk->set_direction(pt);
          for (int i = 0; i < 40; i++)
            {
              trk->set_hits(i, wrap->get_hits(i, itrk));
            }
          AddTrack(trk, itrk);
          delete trk;
        }
      set_DchNTrack(wrap->RowCount());
    }
}
