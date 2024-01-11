#include "TecHit.hh"
#include "TecOutV1.hh"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecOutV1)

using namespace std;

TecOutV1::TecOutV1()
{
  NumberOfHits = 0;
  NumberOfTracks = 0;
  TecHits = new TClonesArray("TecHit", TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrack", TECMAXNUMTRACKS);
  RunNumber = 0;
}

TecOutV1::~TecOutV1()
{
  Clear();
  delete TecHits;
  delete TecTracks;
}

void
TecOutV1::Clear(Option_t *option)
{
  TecHits->Clear();
  TecTracks->Clear();
  if (TecHits->GetSize() > TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfHits = 0;
  NumberOfTracks = 0;
  RunNumber = 0;
}

void
TecOutV1::Reset()
{
  Clear();
}

void
TecOutV1::ClearHits(Option_t *option)
{
  TecHits->Clear();
  if (TecHits->GetSize() > TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void
TecOutV1::ClearTracks(Option_t *option)
{
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

int
TecOutV1::AddTecHit(int iindex, int iwire, int ibin,
                    int adc, float charge, float* xyz, int itrack)
{
  if (NumberOfHits < TecHits->GetSize())
    {
      TClonesArray &techits = *TecHits;
      new(techits[NumberOfHits]) TecHit(iindex, iwire, ibin, adc, 
					charge, xyz, itrack);
      NumberOfHits++;
    }
  else
    {
      int MaxNumberOfHits = TecHits->GetSize() + TECMAXNUMHITS;
      TecHits->Expand(MaxNumberOfHits);
      TClonesArray &techits = *TecHits;
      new(techits[NumberOfHits]) TecHit(iindex, iwire, ibin, adc, 
					charge, xyz, itrack);
      NumberOfHits++;
    }

  return NumberOfHits;
}

int
TecOutV1::AddTecTrack(float* xyzin, float* xyzout)
{
  if (NumberOfTracks < TecTracks->GetSize())
    {
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrack(xyzin, xyzout);
      NumberOfTracks++;
    }
  else
    {
      int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
      TecTracks->Expand(MaxNumberOfTracks);
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrack(xyzin, xyzout);
      NumberOfTracks++;
    }

  return NumberOfTracks;
}

int
TecOutV1::AddTecTrack(TecTrack &source)
{
  if (NumberOfTracks < TecTracks->GetSize())
    {
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrack(source);
      NumberOfTracks++;
    }
  else
    {
      int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
      TecTracks->Expand(MaxNumberOfTracks);
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrack(source);
      NumberOfTracks++;
    }
  
  return NumberOfTracks;
}

int
TecOutV1::getHitTrackID(int ihit, int nn) const
{
  if (nn < 0 || nn > 2)
    {
      nn = 0;
    }
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void
TecOutV1::setHitTrackID(int ihit, int nn, int trkid)
{
  if (nn < 0 || nn > 2)
    {
      nn = 0;
    }
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn, trkid);
}


int
TecOutV1::getHitTrackID(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void
TecOutV1::setHitTrackID(int ihit, int trkid)
{
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int
TecOutV1::getHitGlobalIndex(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int
TecOutV1::getHitIndex(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int
TecOutV1::getHitSector(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index() / (TECMAXPLANE*TECMAXSIDE);
}

int
TecOutV1::getHitPlane(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE * TECMAXSIDE;
  return (ind - (ind / nss)*nss) / TECMAXSIDE;
}

int
TecOutV1::getHitSide(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index() % TECMAXSIDE;
}

int
TecOutV1::getHitWire(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int
TecOutV1::getHitTimeBin(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int
TecOutV1::getHitADC(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float
TecOutV1::getHitCharge(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void
TecOutV1::setHitADC(int ihit, int adc)
{
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void
TecOutV1::setHitCharge(int ihit, float ch)
{
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float
TecOutV1::getHitX(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void
TecOutV1::setHitX(int ihit, float xx)
{
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float
TecOutV1::getHitY(int ihit) const
{
  TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void
TecOutV1::setHitY(int ihit, float yy)
{
  ((TecHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

int
TecOutV1::getTrackSector(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex() / 2;
}

int
TecOutV1::getTrackSide(int itrack) const
{
    if(itrack>=0 && itrack<NumberOfTracks)
    {
      TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
      return tectrack->getIndex() % 2;
    }

  cout << PHWHERE
       << ": Info about unknown TEC track (" << itrack
       << ") requested." << endl;

  return -999;
}

int
TecOutV1::getTrackIndex(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

void
TecOutV1::setTrackIndex(int itrack, int ind)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setIndex(ind);
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind / 2);
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind % 2);
}

float
TecOutV1::getTrackXin(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}

float
TecOutV1::getTrackXinError(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float
TecOutV1::getTrackYin(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}

float
TecOutV1::getTrackYinError(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float
TecOutV1::getTrackXout(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}

float
TecOutV1::getTrackXoutError(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float
TecOutV1::getTrackYout(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}

float
TecOutV1::getTrackYoutError(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int
TecOutV1::getTrackNhits(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  int nh = tectrack->getNhits();
  return nh;
}

int
TecOutV1::getTrackNhits(int itrack, int plane) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

void
TecOutV1::setTrackXin(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setXin(a);
}

void
TecOutV1::setTrackXout(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setXout(a);
}

void
TecOutV1::setTrackYin(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setYin(a);
}

void
TecOutV1::setTrackYout(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setYout(a);
}

void
TecOutV1::setTrackNhits(int itrack, int nh)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNhits(nh);
}

void
TecOutV1::setTrackNhits(int itrack, int iplane, int nh)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane, nh);
}

int
TecOutV1::getTrackNwires(int itrack, int iplane) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

void
TecOutV1::setTrackNwires(int itrack, int iplane, int nw)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane, nw);
}

float
TecOutV1::getTrackdEdX(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float
TecOutV1::getTrackdEdX(int itrack, int iplane) const
{
  float dedx=0.;
  if(itrack<NumberOfTracks && iplane<TECMAXPLANE) {
  if(itrack>-1 && iplane>-1) {
    for(int i=0; i<NumberOfHits; i++) {
      TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(itrack);
      if(techit->get_trackid()==itrack && techit->get_plane()==iplane) {
        dedx+=techit->get_charge();
      }
    }
  }
  }
    return dedx;
}

void
TecOutV1::setTrackdEdX(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setdEdX(a);
}

float
TecOutV1::getTrackLength(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

void
TecOutV1::setTrackLength(int itrack, float a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
}

int
TecOutV1::getTrackNdEdXbins(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins();
}

void
TecOutV1::setTrackNdEdXbins(int itrack, int a)
{
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(a);
}

float
TecOutV1::getTrackAlpha(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float
TecOutV1::getTrackPhi(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float
TecOutV1::getTrackCharge(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if (alpha > 0)
    {
      return 1.0;
    }
  else
    {
      return -1.0;
    }
}

float
TecOutV1::getTrackPt(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float
TecOutV1::getTrackSlope(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float
TecOutV1::getTrackIntercept(int itrack) const
{
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

void
TecOutV1::identify(ostream& out) const
{
  out << "I am a TecOutV1 object." << endl;
}

int 
TecOutV1::getMaxNHits() const
{
  return TecHits->GetSize();
}

int 
TecOutV1::getMaxNTracks() const
{
  return TecTracks->GetSize();
}
