#include "TecOutV5.hh"
#include "TecShortHit.hh"
#include "TecTrackV2.hh"
#include "TecTrackTRv3.hh"
#include "mTecUtilities.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecOutV5)

using namespace std;

static unsigned int TECMAXNUMHITS = 2000;
static unsigned int TECMAXNUMTRACKS = 10;


TecOutV5::TecOutV5(unsigned int typetrack) {
  NumberOfHits=0;
  NumberOfTracks=0;
  TecHits = new TClonesArray("TecShortHit",TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrackV2",TECMAXNUMTRACKS);
  RunNumber=0;
}

TecOutV5::~TecOutV5() {
  Clear();
  delete TecHits;
  delete TecTracks;
}

void TecOutV5::Clear(Option_t *option) {
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

void TecOutV5::ClearHits(Option_t *option) {
  TecHits->Clear();
  if (TecHits->GetSize() > (int) TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void TecOutV5::ClearTracks(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > (int) TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV5::Reset() {
  Clear();
}

int TecOutV5::AddTecHit(int iindex, int iwire, int ibin,
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

int TecOutV5::AddTecTrack(float* xyzin, float* xyzout) {
    
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

int TecOutV5::AddTecTrack(TecTrack &source) {

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

int TecOutV5::getHitTrackID(int ihit, int nn) const {
  if(nn<0 || nn>2) nn=0;
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void TecOutV5::setHitTrackID(int ihit, int nn, int trkid) {
  if(nn<0 || nn>2) nn=0;
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn,trkid);
}

int TecOutV5::getHitTrackID(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void TecOutV5::setHitTrackID(int ihit, int trkid) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int TecOutV5::getHitGlobalIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int TecOutV5::getHitIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int TecOutV5::getHitSector(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()/(TECMAXPLANE*TECMAXSIDE);
}

int TecOutV5::getHitPlane(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE*TECMAXSIDE;
  return (ind - (ind/nss)*nss)/TECMAXSIDE;
}

int TecOutV5::getHitSide(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()%TECMAXSIDE;
}

int TecOutV5::getHitWire(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int TecOutV5::getHitTimeBin(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int TecOutV5::getHitADC(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float TecOutV5::getHitCharge(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void TecOutV5::setHitADC(int ihit, int adc) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void TecOutV5::setHitCharge(int ihit, float ch) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float TecOutV5::getHitX(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void TecOutV5::setHitX(int ihit, float xx) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float TecOutV5::getHitY(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void TecOutV5::setHitY(int ihit, float yy) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

//=============================================================================

int TecOutV5::getTrackSector(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSector();
}

int TecOutV5::getTrackSide(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSide();
}

int TecOutV5::getTrackIndex(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

float TecOutV5::getTrackXin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}
float TecOutV5::getTrackXinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float TecOutV5::getTrackYin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}
float TecOutV5::getTrackYinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float TecOutV5::getTrackXout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}
float TecOutV5::getTrackXoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float TecOutV5::getTrackYout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}
float TecOutV5::getTrackYoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int TecOutV5::getTrackNhits(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits = 0;
  for (int k=0; k<6; k++)
    totnhits += tectrack->getNHITS(k);
  return totnhits;
}

int TecOutV5::getTrackNhits100(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits100 = 0;
  for (int k=0; k<6; k++)
    totnhits100 += tectrack->getNhits100(k);
  return totnhits100;
}

int TecOutV5::getTrackNhits100(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits100(iplane);
}

int TecOutV5::getTrackNhits20(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits20 = 0;
  for (int k=0; k<6; k++)
    totnhits20 += tectrack->getNhits20(k);
  return totnhits20;
}

int TecOutV5::getTrackNhits20(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits20(iplane);
}

int TecOutV5::getTrackNhits(int itrack, int plane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

int TecOutV5::getTrackNtr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totntr = 0;
  for (int k=0; k<6 ; k++)
    totntr += tectrack->getNTR(k);
  return totntr;
}

int TecOutV5::getTrackNtr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNTR(iplane);
}

int TecOutV5::getTrackNwires(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

float TecOutV5::getTrackDE(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float TecOutV5::getTrackTr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float tottr = 0;
  for (int k=0; k<6 ; k++)
    tottr += tectrack->getTR(k);
  return tottr;
}

float TecOutV5::getTrackdEdX06(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totdedx06 = 0;
  for (int k=0; k<6 ; k++)
    totdedx06 += tectrack->getdEdX06(k);
  return totdedx06;
}

float TecOutV5::getTrackdEdX06(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX06(iplane);
}

float TecOutV5::getTrackDE(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getDE(iplane);
}

float TecOutV5::getTrackTr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTR(iplane);
}

float TecOutV5::getTrackLength(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

int TecOutV5::getTrackNdEdXbins(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins(iplane);
}

int TecOutV5::getTrackNdEdXbins(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totndedxbins = 0;
  for (int k=0; k<6; k++)
    totndedxbins += tectrack->getNdEdXbins(k);
  return totndedxbins;
}

float TecOutV5::getTrackChi2(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getChi2();
}

float TecOutV5::getTrackAlpha(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float TecOutV5::getTrackPhi(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float TecOutV5::getTrackCharge(int itrack) const {
  cout << "TecOutV5::getTrackCharge WARNING: returning sign of Alpha angle!" << endl;
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if(alpha>0) return 1.; else return -1.;
}

float TecOutV5::getTrackPt(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float TecOutV5::getTrackSlope(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float TecOutV5::getTrackIntercept(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

float TecOutV5::getTrackWeightedTimeBin(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBin(iplane);
}

float TecOutV5::getTrackWeightedTimeBin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totweightedtime = 0.;
  for (int k=0; k<6; k++)
    totweightedtime += tectrack->getWeightedTimeBin(k);
  return totweightedtime;
}

float TecOutV5::getTrackLikelihood(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getLikelihood();
}

int TecOutV5::getTrackPc3Pointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Pointer();
}

float TecOutV5::getTrackPc3Distance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Distance();
}

float TecOutV5::getTrackPc3Z(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3Z();
}

int TecOutV5::getTrackPc3sPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sPointer();
}

float TecOutV5::getTrackPc3sDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sDistance();
}

int TecOutV5::getTrackPc1Pointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Pointer();
}

float TecOutV5::getTrackPc1Distance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Distance();
}

float TecOutV5::getTrackPc1Z(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1Z();
}

int TecOutV5::getTrackPc1sPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1sPointer();
}

float TecOutV5::getTrackPc1sDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc1sDistance();
}

int TecOutV5::getTrackEmcPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcPointer();
}

float TecOutV5::getTrackEmcDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcDistance();
}

float TecOutV5::getTrackEmcZ(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcZ();
}

float TecOutV5::getTrackEmcEcore(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcEcore();
}

int TecOutV5::getTrackEmcsPointer(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sPointer();
}

float TecOutV5::getTrackEmcsDistance(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPc3sDistance();
}

float TecOutV5::getTrackEmcsEcore(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getEmcsEcore();
}

int TecOutV5::getTrackCrkN0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkN0();
}

float TecOutV5::getTrackCrkNpe0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkNpe0();
}

float TecOutV5::getTrackCrkChi2(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkChi2();
}

float TecOutV5::getTrackCrkDist(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrkDist();
}

int TecOutV5::getTrackCrksN0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksN0();
}

float TecOutV5::getTrackCrksNpe0(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksNpe0();
}

float TecOutV5::getTrackCrksChi2(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksChi2();
}

float TecOutV5::getTrackCrksDist(int itrack) const {
  TecTrackV2* tectrack = (TecTrackV2*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getCrksDist();
}

void TecOutV5::identify(ostream& out) const {
  out << "I am a TecOutV5 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
  return;
}

void TecOutV5::setTrackNhits(int itrack, int i) {
 ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits(i);
  return;
}


void TecOutV5::setTrackIndex(int itrack, int ind) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind/2);
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind%2);
  return;
}

void TecOutV5::setTrackChi2(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setChi2(a);
  return;
}

void TecOutV5::setTrackAlpha(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setAlpha(a);
  return;
}

void TecOutV5::setTrackPhi(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setPhi(a);
  return;
}

void TecOutV5::setTrackLength(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
  return;
}

void TecOutV5::setTrackNdEdXbins(int itrack, int iplane, int a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(iplane, a);
  return;
}

void TecOutV5::setTrackDE(int itrack, int iplane, float de) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setDE(iplane,de);
  return;
}

void TecOutV5::setTrackTr(int itrack, int iplane, float tr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTR(iplane,tr);
  return;
}

void TecOutV5::setTrackdEdX06(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX06(iplane, a);
  return;
}

void TecOutV5::setTrackNwires(int itrack, int iplane, int nw) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane,nw);
  return;
}

void TecOutV5::setTrackNhits100(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits100(iplane, nh);
  return;
}

void TecOutV5::setTrackNhits20(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits20(iplane, nh);
  return;
}

void TecOutV5::setTrackNhits(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane,nh);
  return;
}

void TecOutV5::setTrackNtr(int itrack, int iplane, int ntr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNTR(iplane,ntr);
  return;
}

void TecOutV5::setTrackWeightedTimeBin(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBin(iplane, a);
  return;
}

void TecOutV5::setTrackLikelihood(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setLikelihood(a);
  return;
}

 void
 TecOutV5::setTrackXin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXin(a);
 }

 void
 TecOutV5::setTrackXout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXout(a);
 }

 void
 TecOutV5::setTrackYin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYin(a);
 }

 void
 TecOutV5::setTrackYout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYout(a);
 }

//------------------------  microDST methods -------------------------

int TecOutV5::isValid() const {
  return((NumberOfTracks>0 || NumberOfHits>0) ? 1 : 0);
}

int TecOutV5::get_index(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getIndex() : -999);
}


void TecOutV5::set_index(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setIndex(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}


int TecOutV5::get_nhits(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getNhits() : -999);
}

void TecOutV5::set_nhits(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setNhits(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV5::get_pt(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->gettecMomentum() : -999);
}

void TecOutV5::set_pt(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV5::get_xin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXin() : -999);
}

void TecOutV5::set_xin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV5::get_xout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXout() : -999);
}

void TecOutV5::set_xout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV5::get_yin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYin() : -999);
}

void TecOutV5::set_yin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV5::get_yout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYout() : -999);
}

void TecOutV5::set_yout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV5::get_phi(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPhi() : -999);
}

void TecOutV5::set_phi(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV5::get_alpha(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getAlpha() : -999);
}

void TecOutV5::set_alpha(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}


float TecOutV5::get_dEdx1(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float trklen = tectrk->getTrackLength();
  float dE = tectrk->getdEdX();
    if(trklen>0) { return dE/trklen; }
      else { return -999.; }
}

void TecOutV5::set_dEdx1(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

float TecOutV5::get_dEdx2(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float nbins = (float)tectrk->getNdEdXbins();
  float dE = tectrk->getdEdX();
    if(nbins>0) { return dE/nbins; }
      else { return -999.; }
}

void TecOutV5::set_dEdx2(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

// short int TecOutV5::get_Pc1Hit(const unsigned int itrk) const {
//   cout << PHWHERE << "ERROR This method is not available in TecOutV5(TecTrackV2)." << endl;
//   return -1;
// }

void TecOutV5::set_Pc1Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV5::get_Pc3Hit(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPc3Pointer() : -999);
}

void TecOutV5::set_Pc3Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV5::get_TofHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV5(TecTrackTRv2)." << endl;
  return -1;
}

void TecOutV5::set_TofHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV5::get_EmcHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV5(TecTrackTRv2)." << endl;
  return -1;
}

void TecOutV5::set_EmcHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

int TecOutV5::getMaxNHits() const {
  return TecHits->GetSize();
}

int TecOutV5::getMaxNTracks() const {
  return TecTracks->GetSize();
}


