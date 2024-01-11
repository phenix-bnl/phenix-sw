#include <TecOutV4.hh>
#include <TecShortHit.hh>
#include <TecTrackTRv2.hh>
#include <mTecUtilities.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(TecOutV4);

using namespace std;

static int TECMAXNUMHITS = 2000;
static int TECMAXNUMTRACKS = 10;


TecOutV4::TecOutV4()
{
  NumberOfHits = 0;
  NumberOfTracks = 0;
  TecHits = new TClonesArray("TecShortHit", TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrackTRv2", TECMAXNUMTRACKS);
  RunNumber = 0;
}

TecOutV4::~TecOutV4()
{
  Clear();
  delete TecHits;
  delete TecTracks;
}

void
TecOutV4::Clear(Option_t *option)
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
TecOutV4::ClearHits(Option_t *option)
{
  TecHits->Clear();
  if (TecHits->GetSize() > TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void
TecOutV4::ClearTracks(Option_t *option)
{
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void
TecOutV4::Reset()
{
  Clear();
}

int
TecOutV4::AddTecHit(int iindex, int iwire, int ibin,
                    int adc, float charge, float* xyz, int itrack)
{

  if (NumberOfHits < TecHits->GetSize())
    {
      TClonesArray &techits = *TecHits;
      new(techits[NumberOfHits]) TecShortHit(iindex, iwire, ibin, adc,
                                             charge, xyz, itrack);
      NumberOfHits++;
    }
  else
    {
      int MaxNumberOfHits = TecHits->GetSize() + TECMAXNUMHITS;
      TecHits->Expand(MaxNumberOfHits);
      TClonesArray &techits = *TecHits;
      new(techits[NumberOfHits]) TecShortHit(iindex, iwire, ibin, adc,
                                             charge, xyz, itrack);
      NumberOfHits++;
    }

  return NumberOfHits;
}

int
TecOutV4::AddTecTrack(float* xyzin, float* xyzout)
{

  if (NumberOfTracks < TecTracks->GetSize())
    {
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(xyzin, xyzout);
      NumberOfTracks++;
    }
  else
    {
      int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
      TecTracks->Expand(MaxNumberOfTracks);
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(xyzin, xyzout);
      NumberOfTracks++;
    }

  return NumberOfTracks;
}

int
TecOutV4::AddTecTrack(TecTrack &source)
{

  if (NumberOfTracks < TecTracks->GetSize())
    {
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(source);
      NumberOfTracks++;
    }
  else
    {
      int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
      TecTracks->Expand(MaxNumberOfTracks);
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(source);
      NumberOfTracks++;
    }

  return NumberOfTracks;
}

int
TecOutV4::AddTecTrack(TecTrackTRv2 &source)
{
  if (NumberOfTracks < TecTracks->GetSize())
    {
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(source);
      NumberOfTracks++;
    }
  else
    {
      int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
      TecTracks->Expand(MaxNumberOfTracks);
      TClonesArray &tectracks = *TecTracks;
      new(tectracks[NumberOfTracks]) TecTrackTRv2(source);
      NumberOfTracks++;
    }

  return NumberOfTracks;
}

int
TecOutV4::getHitTrackID(int ihit, int nn) const
{
  if (nn < 0 || nn > 2)
    nn = 0;
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void
TecOutV4::setHitTrackID(int ihit, int nn, int trkid)
{
  if (nn < 0 || nn > 2)
    nn = 0;
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn, trkid);
}

int
TecOutV4::getHitTrackID(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void
TecOutV4::setHitTrackID(int ihit, int trkid)
{
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int
TecOutV4::getHitGlobalIndex(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int
TecOutV4::getHitIndex(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int
TecOutV4::getHitSector(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index() / (TECMAXPLANE*TECMAXSIDE);
}

int
TecOutV4::getHitPlane(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE * TECMAXSIDE;
  return (ind - (ind / nss)*nss) / TECMAXSIDE;
}

int
TecOutV4::getHitSide(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index() % TECMAXSIDE;
}

int
TecOutV4::getHitWire(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int
TecOutV4::getHitTimeBin(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int
TecOutV4::getHitADC(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float
TecOutV4::getHitCharge(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void
TecOutV4::setHitADC(int ihit, int adc)
{
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void
TecOutV4::setHitCharge(int ihit, float ch)
{
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float
TecOutV4::getHitX(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void
TecOutV4::setHitX(int ihit, float xx)
{
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float
TecOutV4::getHitY(int ihit) const
{
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void
TecOutV4::setHitY(int ihit, float yy)
{
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

int
TecOutV4::getTrackSector(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSector();
}

int
TecOutV4::getTrackSide(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSide();
}

int
TecOutV4::getTrackIndex(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

float
TecOutV4::getTrackXin(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}

float
TecOutV4::getTrackXinError(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float
TecOutV4::getTrackYin(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}

float
TecOutV4::getTrackYinError(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float
TecOutV4::getTrackXout(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}

float
TecOutV4::getTrackXoutError(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float
TecOutV4::getTrackYout(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}

float TecOutV4::getTrackYoutError(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int
TecOutV4::getTrackNhits(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits();
}

int
TecOutV4::getTrackNhits100(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits100();
}

int
TecOutV4::getTrackNhits200(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits200();
}

int
TecOutV4::getTrackNhits50(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits50();
}

int
TecOutV4::getTrackNhits(int itrack, int plane) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

int
TecOutV4::getTrackNtr(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNtr();
}

int
TecOutV4::getTrackNtr(int itrack, int plane) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNTR(plane);
}

int
TecOutV4::getTrackNwires(int itrack, int iplane) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

float
TecOutV4::getTrackdEdX(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float
TecOutV4::getTrackTr(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTr();
}

float
TecOutV4::getTrackdEdX06(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX06();
}

float
TecOutV4::getTrackdEdX(int itrack, int iplane) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getDE(iplane);
}

float
TecOutV4::getTrackTr(int itrack, int iplane) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTR(iplane);
}

float
TecOutV4::getTrackLength(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

int
TecOutV4::getTrackNdEdXbins(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins();
}

float
TecOutV4::getTrackAlpha(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float
TecOutV4::getTrackPhi(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float
TecOutV4::getTrackCharge(int itrack) const
{
  cout << "TecOutV4::getTrackCharge WARNING: returning sign of Alpha angle!" << endl;
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();

  return (alpha > 0) ? 1.0 : -1.0;
}

float
TecOutV4::getTrackPt(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float
TecOutV4::getTrackSlope(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float
TecOutV4::getTrackIntercept(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

float
TecOutV4::getTrackWeightedTimeBin(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBin();
}

float
TecOutV4::getTrackWeightedTimeBinSq(int itrack) const
{
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBinSq();
}

void
TecOutV4::identify(ostream& out) const
{
  out << "I am a TecOutV4 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
  return ;
}

void
TecOutV4::setTrackIndex(int itrack, int ind)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind / 2);
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind % 2);
  return ;
}

void
TecOutV4::setTrackLength(int itrack, float a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
  return ;
}

void
TecOutV4::setTrackNdEdXbins(int itrack, int a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(a);
  return ;
}

void
TecOutV4::setTrackdEdX(int itrack, float a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX(a);
  return ;
}

void
TecOutV4::setTrackdEdX(int itrack, int iplane, float de)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setDE(iplane, de);
  return ;
}

void
TecOutV4::setTrackTr(int itrack, float tr)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTr(tr);
  return ;
}

void
TecOutV4::setTrackTr(int itrack, int iplane, float tr)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTR(iplane, tr);
  return ;
}

void
TecOutV4::setTrackdEdX06(int itrack, float a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX06(a);
  return ;
}

void
TecOutV4::setTrackNwires(int itrack, int iplane, int nw)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane, nw);
  return ;
}

void
TecOutV4::setTrackNhits(int itrack, int nh)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits(nh);
  return ;
}

void
TecOutV4::setTrackNhits100(int itrack, int nh)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits100(nh);
  return ;
}

void
TecOutV4::setTrackNhits200(int itrack, int nh)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits200(nh);
  return ;
}

void
TecOutV4::setTrackNhits50(int itrack, int nh)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits50(nh);
  return ;
}

void
TecOutV4::setTrackNtr(int itrack, int ntr)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNtr(ntr);
  return ;
}

void
TecOutV4::setTrackNhits(int itrack, int iplane, int nh)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane, nh);
  return ;
}

void
TecOutV4::setTrackNtr(int itrack, int iplane, int ntr)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNTR(iplane, ntr);
  return ;
}

void
TecOutV4::setTrackWeightedTimeBin(int itrack, float a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBin(a);
  return ;
}

void
TecOutV4::setTrackWeightedTimeBinSq(int itrack, float a)
{
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBinSq(a);
  return ;
}

int
TecOutV4::isValid() const
{
  return ((NumberOfTracks > 0 || NumberOfHits > 0) ? 1 : 0);
}

int
TecOutV4::get_index(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getIndex() : -999);
}


void
TecOutV4::set_index(const unsigned int itrk, const int ival)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setIndex(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

int
TecOutV4::get_nhits(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getNhits() : -999);
}

void
TecOutV4::set_nhits(const unsigned int itrk, const int ival)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setNhits(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

float
TecOutV4::get_pt(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->gettecMomentum() : -999);
}

void
TecOutV4::set_pt(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return ;
}

float
TecOutV4::get_xin(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getXin() : -999);
}

void
TecOutV4::set_xin(const unsigned int itrk, const float rval)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setXin(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

float
TecOutV4::get_xout(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getXout() : -999);
}

void
TecOutV4::set_xout(const unsigned int itrk, const float rval)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setXout(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

float
TecOutV4::get_yin(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getYin() : -999);
}

void
TecOutV4::set_yin(const unsigned int itrk, const float rval)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setYin(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

float
TecOutV4::get_yout(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getYout() : -999);
}

void
TecOutV4::set_yout(const unsigned int itrk, const float rval)
{
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk)
    {
      tectrk->setYout(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no TecTrack object found." << endl;
    }
  return ;
}

float
TecOutV4::get_phi(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getPhi() : -999);
}

void
TecOutV4::set_phi(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return ;
}

float
TecOutV4::get_alpha(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getAlpha() : -999);
}

void
TecOutV4::set_alpha(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return ;
}

float
TecOutV4::get_dEdx1(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float trklen = tectrk->getTrackLength();
  float dE = tectrk->getdEdX();
  if (trklen > 0)
    {
      return dE / trklen;
    }
  else
    {
      return -999.0;
    }
}

void
TecOutV4::set_dEdx1(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return ;
}

float
TecOutV4::get_dEdx2(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float nbins = (float)tectrk->getNdEdXbins();
  float dE = tectrk->getdEdX();
  if (nbins > 0)
    {
      return dE / nbins;
    }
  else
    {
      return -999.0;
    }
}

void
TecOutV4::set_dEdx2(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return ;
}

short int
TecOutV4::get_Pc1Hit(const unsigned int itrk) const
{
  cout << PHWHERE
       << "ERROR This method is not available in TecOutV4(TecTrackTRv2)."
       << endl;

  return -1;
}

void
TecOutV4::set_Pc1Hit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return ;
}

short int
TecOutV4::get_Pc3Hit(const unsigned int itrk) const
{
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return ((tectrk) ? tectrk->getPc3Pointer() : -999);
}

void
TecOutV4::set_Pc3Hit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return ;
}

short int
TecOutV4::get_TofHit(const unsigned int itrk) const
{
  cout << PHWHERE << "ERROR This method is not available in TecOutV4(TecTrackTRv2)." << endl;
  return -1;
}

void
TecOutV4::set_TofHit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return ;
}

short int
TecOutV4::get_EmcHit(const unsigned int itrk) const
  {
    cout << PHWHERE
    << "ERROR This method is not available in TecOutV4(TecTrackTRv2)."
    << endl;

    return -1;
  }

void
TecOutV4::set_EmcHit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;

  return ;
}

int
TecOutV4::getMaxNHits() const
  {
    return TecHits->GetSize();
  }

int
TecOutV4::getMaxNTracks() const
  {
    return TecTracks->GetSize();
  }
