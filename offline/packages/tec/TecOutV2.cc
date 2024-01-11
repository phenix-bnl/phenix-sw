#include "TecOutV2.hh"
#include "TecShortHit.hh"
#include "TecTrack.hh"
#include "mTecUtilities.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecOutV2)

using namespace std;

TecOutV2::TecOutV2() {

  NumberOfHits=0;
  NumberOfTracks=0;
  TecHits = new TClonesArray("TecShortHit",TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrack",TECMAXNUMTRACKS);
  RunNumber=0;

}

TecOutV2::~TecOutV2() {
  Clear();
  delete TecHits;
  delete TecTracks;
}

void TecOutV2::Clear(Option_t *option) {
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
  RunNumber=0;
}

void TecOutV2::ClearHits(Option_t *option) {
  TecHits->Clear();
  if (TecHits->GetSize() > TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void TecOutV2::ClearTracks(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV2::Reset() {
  Clear();
}

int TecOutV2::AddTecHit(int iindex, int iwire, int ibin,
                        int adc, float charge, float* xyz, int itrack) {

  if(NumberOfHits < TecHits->GetSize()) {
    TClonesArray &techits = *TecHits;
    new(techits[NumberOfHits]) TecShortHit(iindex, iwire, ibin, adc, charge, xyz, itrack);
    NumberOfHits++;
  }
  else {
    int MaxNumberOfHits = TecHits->GetSize() + TECMAXNUMHITS;
    TecHits->Expand(MaxNumberOfHits);
    TClonesArray &techits = *TecHits;
    new(techits[NumberOfHits]) TecShortHit(iindex, iwire, ibin, adc, charge, xyz, itrack);
    NumberOfHits++;
  } 

  return NumberOfHits;
}

int TecOutV2::AddTecTrack(float* xyzin, float* xyzout) {
    
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrack(xyzin, xyzout);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrack(xyzin, xyzout);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV2::AddTecTrack(TecTrack &source) {

  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrack(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrack(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

//=============================================================================

int TecOutV2::getHitTrackID(int ihit, int nn) const {
  if(nn<0 || nn>2) nn=0;
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void TecOutV2::setHitTrackID(int ihit, int nn, int trkid) {
  if(nn<0 || nn>2) nn=0;
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn,trkid);
}

int TecOutV2::getHitTrackID(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void TecOutV2::setHitTrackID(int ihit, int trkid) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int TecOutV2::getHitGlobalIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int TecOutV2::getHitIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int TecOutV2::getHitSector(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()/(TECMAXPLANE*TECMAXSIDE);
}

int TecOutV2::getHitPlane(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE*TECMAXSIDE;
  return (ind - (ind/nss)*nss)/TECMAXSIDE;
}

int TecOutV2::getHitSide(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()%TECMAXSIDE;
}

int TecOutV2::getHitWire(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int TecOutV2::getHitTimeBin(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int TecOutV2::getHitADC(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float TecOutV2::getHitCharge(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void TecOutV2::setHitADC(int ihit, int adc) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void TecOutV2::setHitCharge(int ihit, float ch) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float TecOutV2::getHitX(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void TecOutV2::setHitX(int ihit, float xx) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float TecOutV2::getHitY(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void TecOutV2::setHitY(int ihit, float yy) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

//=============================================================================

int TecOutV2::getTrackSector(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex()/2;
}

int TecOutV2::getTrackSide(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex()%2;
}

int TecOutV2::getTrackIndex(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

void TecOutV2::setTrackIndex(int itrack, int ind) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setIndex(ind);
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind/2);
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind%2);
}

float TecOutV2::getTrackXin(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}
float TecOutV2::getTrackXinError(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float TecOutV2::getTrackYin(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}
float TecOutV2::getTrackYinError(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float TecOutV2::getTrackXout(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}
float TecOutV2::getTrackXoutError(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float TecOutV2::getTrackYout(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}
float TecOutV2::getTrackYoutError(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int TecOutV2::getTrackNhits(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  int nh = tectrack->getNhits();
  return nh;
}

int TecOutV2::getTrackNhits(int itrack, int plane) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

void TecOutV2::setTrackNhits(int itrack, int nh) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNhits(nh);
}

void TecOutV2::setTrackNhits(int itrack, int iplane, int nh) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane,nh);
}

int TecOutV2::getTrackNwires(int itrack, int iplane) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

void TecOutV2::setTrackNwires(int itrack, int iplane, int nw) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane,nw);
}

float TecOutV2::getTrackdEdX(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float TecOutV2::getTrackdEdX(int itrack, int iplane) const
{
  // tec short hit does not have charge: calculate it from adc
  float dedx=0.;
  if(itrack<NumberOfTracks && iplane<TECMAXPLANE) {
  if(itrack>-1 && iplane>-1) {
    for(int i=0; i<NumberOfHits; i++) {
      TecHit* techit = (TecHit*)GetTecHits()->UncheckedAt(itrack);
      if(techit->get_trackid()==itrack && techit->get_plane()==iplane) {
        int adc = techit->get_adc();
        float charge = TecUtilities::Ampl2Charge(adc);
        dedx+=charge;
      }
    }
  }
  }
    return dedx;
}

void TecOutV2::setTrackdEdX(int itrack, float a) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setdEdX(a);
}

float TecOutV2::getTrackLength(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

void TecOutV2::setTrackLength(int itrack, float a) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
}

int TecOutV2::getTrackNdEdXbins(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins();
}

void TecOutV2::setTrackNdEdXbins(int itrack, int a) {
  ((TecTrack*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(a);
}

float TecOutV2::getTrackAlpha(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float TecOutV2::getTrackPhi(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float TecOutV2::getTrackCharge(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if(alpha>0) return 1.; else return -1.;
}

float TecOutV2::getTrackPt(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float TecOutV2::getTrackSlope(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float TecOutV2::getTrackIntercept(int itrack) const {
  TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

void TecOutV2::identify(ostream& out) const {
  out << "I am a TecOutV2 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
}


//------------------------  microDST methods -------------------------

int TecOutV2::isValid() const
{
  return((NumberOfTracks>0 || NumberOfHits>0) ? 1 : 0);
}

int TecOutV2::get_index(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getIndex() : -999);
}


void TecOutV2::set_index(const unsigned int itrk, const int ival)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setIndex(ival);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}


int TecOutV2::get_nhits(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getNhits() : -999);
}

void TecOutV2::set_nhits(const unsigned int itrk, const int ival)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setNhits(ival);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}


float TecOutV2::get_pt(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->gettecMomentum() : -999);
}

void TecOutV2::set_pt(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV2::get_xin(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXin() : -999);
}

void TecOutV2::set_xin(const unsigned int itrk, const float rval)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setXin(rval);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}

float TecOutV2::get_xout(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXout() : -999);
}

void TecOutV2::set_xout(const unsigned int itrk, const float rval)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setXout(rval);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}

float TecOutV2::get_yin(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYin() : -999);
}

void TecOutV2::set_yin(const unsigned int itrk, const float rval)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setYin(rval);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}

float TecOutV2::get_yout(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYout() : -999);
}

void TecOutV2::set_yout(const unsigned int itrk, const float rval)
{
  TecTrack *tectrk = (TecTrack*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) {
      tectrk->setYout(rval);
  }
  else
  {
    cout << PHWHERE << "ERROR no TecTrack object found." << endl;
  }
  return;
}

float TecOutV2::get_phi(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPhi() : -999);
}

void TecOutV2::set_phi(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV2::get_alpha(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getAlpha() : -999);
}

void TecOutV2::set_alpha(const unsigned int itrk, const float rval)
{
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}


float TecOutV2::get_dEdx1(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  float trklen = tectrk->getTrackLength();
  float dE = tectrk->getdEdX();
  if(trklen>0) {
    return dE/trklen;
  }
  else {
    return -999.;
  }
}

void TecOutV2::set_dEdx1(const unsigned int itrk, const float rval)
{ 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

float TecOutV2::get_dEdx2(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  float nbins = (float)tectrk->getNdEdXbins();
  float dE = tectrk->getdEdX();
  if(nbins>0) {
    return dE/nbins;
  }
  else {
    return -999.;
  }
}

void TecOutV2::set_dEdx2(const unsigned int itrk, const float rval)
{ 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}



short int TecOutV2::get_Pc1Hit(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPc1pointer() : -999);
}

void TecOutV2::set_Pc1Hit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV2::get_Pc3Hit(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPc3pointer() : -999);
}

void TecOutV2::set_Pc3Hit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV2::get_TofHit(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getTOFpointer() : -999);
}

void TecOutV2::set_TofHit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV2::get_EmcHit(const unsigned int itrk) const
{
  TecTrack *tectrk = (TecTrack *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getEMCpointer() : -999);
}

void TecOutV2::set_EmcHit(const unsigned int itrk, const short int rval)
{
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

int
TecOutV2::getMaxNHits() const
{
  return TecHits->GetSize();
}

int
TecOutV2::getMaxNTracks() const
{
  return TecTracks->GetSize();
}
