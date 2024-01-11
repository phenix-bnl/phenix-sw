#include "TecOutV7.hh"
#include "TecShortHit.hh"
#include "TecTrackV2.hh"
#include "TecTrackTRv3.hh"
#include "mTecUtilities.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecOutV7)

using namespace std;

static unsigned int TECMAXNUMHITS = 2000;
static unsigned int TECMAXNUMTRACKS = 10;


TecOutV7::TecOutV7() {
  NumberOfHits=0;
  NumberOfTracks=0;
  TecHits = new TClonesArray("TecShortHit",TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrackV2",TECMAXNUMTRACKS);
  RunNumber=0;
}

TecOutV7::~TecOutV7() {
  Clear();
  delete TecHits;
  delete TecTracks;
}

void TecOutV7::Clear(Option_t *option) {
  TecHits->Clear();
  TecTracks->Clear();
  if (TecHits->GetSize() > (int) TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  if (TecTracks->GetSize() > (int) TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfHits = 0;
  NumberOfTracks = 0;
  RunNumber=0;
}

void TecOutV7::ClearHits(Option_t *option) {
  TecHits->Clear();
  if (TecHits->GetSize() > (int) TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void TecOutV7::ClearTracks(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > (int) TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV7::Reset() {
  Clear();
}

int TecOutV7::AddTecHit(int iindex, int iwire, int ibin,
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

int TecOutV7::AddTecTrack(float* xyzin, float* xyzout) {
    
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(xyzin, xyzout);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(xyzin, xyzout);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV7::AddTecTrack(TecTrack &source) {

  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV7::AddTecTrack(TecTrackV2 &source) {
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackV2(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

//=============================================================================

int TecOutV7::getHitTrackID(int ihit, int nn) const {
  if(nn<0 || nn>2) nn=0;
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void TecOutV7::setHitTrackID(int ihit, int nn, int trkid) {
  if(nn<0 || nn>2) nn=0;
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn,trkid);
}

int TecOutV7::getHitTrackID(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void TecOutV7::setHitTrackID(int ihit, int trkid) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int TecOutV7::getHitGlobalIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int TecOutV7::getHitIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int TecOutV7::getHitSector(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()/(TECMAXPLANE*TECMAXSIDE);
}

int TecOutV7::getHitPlane(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE*TECMAXSIDE;
  return (ind - (ind/nss)*nss)/TECMAXSIDE;
}

int TecOutV7::getHitSide(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()%TECMAXSIDE;
}

int TecOutV7::getHitWire(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int TecOutV7::getHitTimeBin(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int TecOutV7::getHitADC(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float TecOutV7::getHitCharge(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void TecOutV7::setHitADC(int ihit, int adc) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void TecOutV7::setHitCharge(int ihit, float ch) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float TecOutV7::getHitX(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void TecOutV7::setHitX(int ihit, float xx) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float TecOutV7::getHitY(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void TecOutV7::setHitY(int ihit, float yy) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

//=============================================================================

int TecOutV7::getTrackSector(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSector();
}

int TecOutV7::getTrackSide(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSide();
}

int TecOutV7::getTrackIndex(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

float TecOutV7::getTrackXin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}
float TecOutV7::getTrackXinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float TecOutV7::getTrackYin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}
float TecOutV7::getTrackYinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float TecOutV7::getTrackXout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}
float TecOutV7::getTrackXoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float TecOutV7::getTrackYout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}
float TecOutV7::getTrackYoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int TecOutV7::getTrackNhits(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits = 0;
  for (int k=0; k<6; k++)
    totnhits += tectrack->getNHITS(k);
  return totnhits;
}

int TecOutV7::getTrackNhits100(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits100 = 0;
  for (int k=0; k<6; k++)
    totnhits100 += tectrack->getNhits100(k);
  return totnhits100;
}

int TecOutV7::getTrackNhits100(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits100(iplane);
}

int TecOutV7::getTrackNhits20(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits20 = 0;
  for (int k=0; k<6; k++)
    totnhits20 += tectrack->getNhits20(k);
  return totnhits20;
}

int TecOutV7::getTrackNhits20(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits20(iplane);
}

int TecOutV7::getTrackNhits(int itrack, int plane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

int TecOutV7::getTrackNtr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totntr = 0;
  for (int k=0; k<6 ; k++)
    totntr += tectrack->getNTR(k);
  return totntr;
}

int TecOutV7::getTrackNtr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNTR(iplane);
}

int TecOutV7::getTrackNwires(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

float TecOutV7::getTrackDE(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float TecOutV7::getTrackTr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float tottr = 0;
  for (int k=0; k<6 ; k++)
    tottr += tectrack->getTR(k);
  return tottr;
}

float TecOutV7::getTrackdEdX06(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totdedx06 = 0;
  for (int k=0; k<6 ; k++)
    totdedx06 += tectrack->getdEdX06(k);
  return totdedx06;
}

float TecOutV7::getTrackdEdX06(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX06(iplane);
}

float TecOutV7::getTrackDE(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getDE(iplane);
}

float TecOutV7::getTrackTr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTR(iplane);
}

float TecOutV7::getTrackLength(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

int TecOutV7::getTrackNdEdXbins(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins(iplane);
}

int TecOutV7::getTrackNdEdXbins(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totndedxbins = 0;
  for (int k=0; k<6; k++)
    totndedxbins += tectrack->getNdEdXbins(k);
  return totndedxbins;
}

float TecOutV7::getTrackChi2(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getChi2();
}

float TecOutV7::getTrackAlpha(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float TecOutV7::getTrackPhi(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float TecOutV7::getTrackCharge(int itrack) const {
  cout << "TecOutV7::getTrackCharge WARNING: returning sign of Alpha angle!" << endl;
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if(alpha>0) return 1.; else return -1.;
}

float TecOutV7::getTrackPt(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float TecOutV7::getTrackSlope(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float TecOutV7::getTrackIntercept(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

float TecOutV7::getTrackWeightedTimeBin(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBin(iplane);
}

float TecOutV7::getTrackWeightedTimeBin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totweightedtime = 0.;
  for (int k=0; k<6; k++)
    totweightedtime += tectrack->getWeightedTimeBin(k);
  return totweightedtime;
}

float TecOutV7::getTrackLikelihood(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getLikelihood();
}

int TecOutV7::getTrackPc3Pointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Pointer();
}

float TecOutV7::getTrackPc3Distance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Distance();
}

float TecOutV7::getTrackPc3Z(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Z();
}

int TecOutV7::getTrackPc3sPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sPointer();
}

float TecOutV7::getTrackPc3sDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sDistance();
}

int TecOutV7::getTrackPc1Pointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Pointer();
}

float TecOutV7::getTrackPc1Distance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Distance();
}

float TecOutV7::getTrackPc1Z(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Z();
}

int TecOutV7::getTrackPc1sPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1sPointer();
}

float TecOutV7::getTrackPc1sDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1sDistance();
}

int TecOutV7::getTrackEmcPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcPointer();
}

float TecOutV7::getTrackEmcDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcDistance();
}

float TecOutV7::getTrackEmcZ(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcZ();
}

float TecOutV7::getTrackEmcEcore(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcEcore();
}

int TecOutV7::getTrackEmcsPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sPointer();
}

float TecOutV7::getTrackEmcsDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sDistance();
}

float TecOutV7::getTrackEmcsEcore(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcsEcore();
}

int TecOutV7::getTrackCrkN0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkN0();
}

float TecOutV7::getTrackCrkNpe0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkNpe0();
}

float TecOutV7::getTrackCrkChi2(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkChi2();
}

float TecOutV7::getTrackCrkDist(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkDist();
}

int TecOutV7::getTrackCrksN0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksN0();
}

float TecOutV7::getTrackCrksNpe0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksNpe0();
}

float TecOutV7::getTrackCrksChi2(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksChi2();
}

float TecOutV7::getTrackCrksDist(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksDist();
}

void TecOutV7::identify(ostream& out) const {
  out << "I am a TecOutV7 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
  return;
}

void TecOutV7::setTrackNhits(int itrack, int i) {
 ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits(i);
  return;
}


void TecOutV7::setTrackIndex(int itrack, int ind) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind/2);
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind%2);
  return;
}

void TecOutV7::setTrackChi2(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setChi2(a);
  return;
}

void TecOutV7::setTrackAlpha(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setAlpha(a);
  return;
}

void TecOutV7::setTrackPhi(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setPhi(a);
  return;
}

void TecOutV7::setTrackLength(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
  return;
}

void TecOutV7::setTrackNdEdXbins(int itrack, int iplane, int a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(iplane, a);
  return;
}

void TecOutV7::setTrackDE(int itrack, int iplane, float de) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setDE(iplane,de);
  return;
}

void TecOutV7::setTrackTr(int itrack, int iplane, float tr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTR(iplane,tr);
  return;
}

void TecOutV7::setTrackdEdX06(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX06(iplane, a);
  return;
}

void TecOutV7::setTrackNwires(int itrack, int iplane, int nw) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane,nw);
  return;
}

void TecOutV7::setTrackNhits100(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits100(iplane, nh);
  return;
}

void TecOutV7::setTrackNhits20(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits20(iplane, nh);
  return;
}

void TecOutV7::setTrackNhits(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane,nh);
  return;
}

void TecOutV7::setTrackNtr(int itrack, int iplane, int ntr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNTR(iplane,ntr);
  return;
}

void TecOutV7::setTrackWeightedTimeBin(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBin(iplane, a);
  return;
}

void TecOutV7::setTrackLikelihood(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setLikelihood(a);
  return;
}

 void
 TecOutV7::setTrackXin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXin(a);
 }

 void
 TecOutV7::setTrackXout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXout(a);
 }

 void
 TecOutV7::setTrackYin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYin(a);
 }

 void
 TecOutV7::setTrackYout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYout(a);
 }

//------------------------  microDST methods -------------------------

int TecOutV7::isValid() const {
  return((NumberOfTracks>0 || NumberOfHits>0) ? 1 : 0);
}

int TecOutV7::get_index(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getIndex() : -999);
}


void TecOutV7::set_index(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setIndex(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}


int TecOutV7::get_nhits(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getNhits() : -999);
}

void TecOutV7::set_nhits(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setNhits(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV7::get_pt(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->gettecMomentum() : -999);
}

void TecOutV7::set_pt(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV7::get_xin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXin() : -999);
}

void TecOutV7::set_xin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV7::get_xout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXout() : -999);
}

void TecOutV7::set_xout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV7::get_yin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYin() : -999);
}

void TecOutV7::set_yin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV7::get_yout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYout() : -999);
}

void TecOutV7::set_yout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV7::get_phi(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPhi() : -999);
}

void TecOutV7::set_phi(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV7::get_alpha(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getAlpha() : -999);
}

void TecOutV7::set_alpha(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}


float TecOutV7::get_dEdx1(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float trklen = tectrk->getTrackLength();
  float dE = tectrk->getdEdX();
    if(trklen>0) { return dE/trklen; }
      else { return -999.; }
}

void TecOutV7::set_dEdx1(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

float TecOutV7::get_dEdx2(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float nbins = (float)tectrk->getNdEdXbins();
  float dE = tectrk->getdEdX();
    if(nbins>0) { return dE/nbins; }
      else { return -999.; }
}

void TecOutV7::set_dEdx2(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

// short int TecOutV7::get_Pc1Hit(const unsigned int itrk) const {
//   cout << PHWHERE << "ERROR This method is not available in TecOutV7(TecTrackV2)." << endl;
//   return -1;
// }

void TecOutV7::set_Pc1Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV7::get_Pc3Hit(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPc3Pointer() : -999);
}

void TecOutV7::set_Pc3Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV7::get_TofHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV7(TecTrackTRv2)." << endl;
  return -1;
}

void TecOutV7::set_TofHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV7::get_EmcHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV7(TecTrackTRv2)." << endl;
  return -1;
}

void TecOutV7::set_EmcHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

int TecOutV7::getMaxNHits() const {
  return TecHits->GetSize();
}

int TecOutV7::getMaxNTracks() const {
  return TecTracks->GetSize();
}


