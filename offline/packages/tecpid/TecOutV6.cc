#include "TecOutV6.hh"
#include "TecTrackTRv3.hh"
#include "mTecUtilities.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecOutV6)

using namespace std;

static int TECMAXNUMTRACKS = 10;


TecOutV6::TecOutV6() {
  NumberOfTracks=0;
  TecTracks = new TClonesArray("TecTrackTRv3",TECMAXNUMTRACKS);
}

TecOutV6::~TecOutV6() {
  Clear();
  delete TecTracks;
}

void TecOutV6::Clear(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV6::ClearTracks(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV6::Reset() {
  Clear();
}

int TecOutV6::AddTecTrack(float* xyzin, float* xyzout) {
    
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(xyzin, xyzout);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(xyzin, xyzout);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV6::AddTecTrack(TecTrack &source) {

  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV6::AddTecTrack(TecTrackTRv3 &source) {
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv3(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

TecOutV6& TecOutV6::operator=(const TecOut &source)
{
   NumberOfTracks = source.getNTracks();
   for (int itrk=0; itrk<NumberOfTracks; itrk++)
     {
       TecTrack* tectrack = (TecTrack*)GetTecTracks()->UncheckedAt(itrk);
       AddTecTrack(*tectrack);
     }
   return *this;
}

//=============================================================================

int TecOutV6::getTrackSector(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSector();
}

int TecOutV6::getTrackSide(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSide();
}

int TecOutV6::getTrackIndex(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

float TecOutV6::getTrackXin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}
float TecOutV6::getTrackXinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float TecOutV6::getTrackYin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}
float TecOutV6::getTrackYinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float TecOutV6::getTrackXout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}
float TecOutV6::getTrackXoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float TecOutV6::getTrackYout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}
float TecOutV6::getTrackYoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int TecOutV6::getTrackNhits(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits = 0;
  for (int k=0; k<6; k++)
    totnhits += tectrack->getNHITS(k);
  return totnhits;
}

int TecOutV6::getTrackNhits100(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits100 = 0;
  for (int k=0; k<6; k++)
    totnhits100 += tectrack->getNhits100(k);
  return totnhits100;
}

int TecOutV6::getTrackNhits100(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits100(iplane);
}

int TecOutV6::getTrackNhits20(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totnhits20 = 0;
  for (int k=0; k<6; k++)
    totnhits20 += tectrack->getNhits20(k);
  return totnhits20;
}

int TecOutV6::getTrackNhits20(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits20(iplane);
}

int TecOutV6::getTrackNhits(int itrack, int plane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

int TecOutV6::getTrackNtr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totntr = 0;
  for (int k=0; k<6 ; k++)
    totntr += tectrack->getNTR(k);
  return totntr;
}

int TecOutV6::getTrackNtr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNTR(iplane);
}

int TecOutV6::getTrackNwires(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

float TecOutV6::getTrackDE(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float TecOutV6::getTrackTr(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float tottr = 0;
  for (int k=0; k<6 ; k++)
    tottr += tectrack->getTR(k);
  return tottr;
}

float TecOutV6::getTrackdEdX06(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totdedx06 = 0;
  for (int k=0; k<6 ; k++)
    totdedx06 += tectrack->getdEdX06(k);
  return totdedx06;
}

float TecOutV6::getTrackdEdX06(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX06(iplane);
}

float TecOutV6::getTrackDE(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getDE(iplane);
}

float TecOutV6::getTrackTr(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTR(iplane);
}

float TecOutV6::getTrackLength(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

int TecOutV6::getTrackNdEdXbins(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins(iplane);
}

int TecOutV6::getTrackNdEdXbins(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  int totndedxbins = 0;
  for (int k=0; k<6; k++)
    totndedxbins += tectrack->getNdEdXbins(k);
  return totndedxbins;
}

float TecOutV6::getTrackChi2(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getChi2();
}

float TecOutV6::getTrackAlpha(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float TecOutV6::getTrackPhi(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float TecOutV6::getTrackCharge(int itrack) const {
  cout << "TecOutV6::getTrackCharge WARNING: returning sign of Alpha angle!" << endl;
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if(alpha>0) return 1.; else return -1.;
}

float TecOutV6::getTrackPt(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float TecOutV6::getTrackSlope(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float TecOutV6::getTrackIntercept(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

float TecOutV6::getTrackWeightedTimeBin(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBin(iplane);
}

float TecOutV6::getTrackWeightedTimeBin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float totweightedtime = 0.;
  for (int k=0; k<6; k++)
    totweightedtime += tectrack->getWeightedTimeBin(k);
  return totweightedtime;
}


float TecOutV6::getTrackLikelihood(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getLikelihood();
}

 void TecOutV6::setTrackXin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXin(a);
 }

 void TecOutV6::setTrackXout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setXout(a);
 }

 void TecOutV6::setTrackYin(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYin(a);
 }

 void TecOutV6::setTrackYout(int itrack, float a)
 {
   ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setYout(a);
 }


void TecOutV6::identify(ostream& out) const {
  out << "I am a TecOutV6 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
  return;
}

void TecOutV6::setTrackNhits(int itrack, int i) {
 ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits(i);
  return;
}


void TecOutV6::setTrackIndex(int itrack, int ind) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind/2);
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind%2);
  return;
}

void TecOutV6::setTrackChi2(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setChi2(a);
  return;
}

void TecOutV6::setTrackAlpha(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setAlpha(a);
  return;
}

void TecOutV6::setTrackPhi(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setPhi(a);
  return;
}

void TecOutV6::setTrackLength(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
  return;
}

void TecOutV6::setTrackNdEdXbins(int itrack, int iplane, int a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(iplane, a);
  return;
}

void TecOutV6::setTrackDE(int itrack, int iplane, float de) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setDE(iplane,de);
  return;
}

void TecOutV6::setTrackTr(int itrack, int iplane, float tr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTR(iplane,tr);
  return;
}

void TecOutV6::setTrackdEdX06(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX06(iplane, a);
  return;
}

void TecOutV6::setTrackNwires(int itrack, int iplane, int nw) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane,nw);
  return;
}

void TecOutV6::setTrackNhits100(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits100(iplane, nh);
  return;
}

void TecOutV6::setTrackNhits20(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits20(iplane, nh);
  return;
}

void TecOutV6::setTrackNhits(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane,nh);
  return;
}

void TecOutV6::setTrackNtr(int itrack, int iplane, int ntr) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNTR(iplane,ntr);
  return;
}

void TecOutV6::setTrackWeightedTimeBin(int itrack, int iplane, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBin(iplane, a);
  return;
}

void TecOutV6::setTrackLikelihood(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setLikelihood(a);
  return;
}

//------------------------  microDST methods -------------------------

int TecOutV6::isValid() const {
  return((NumberOfTracks>0) ? 1 : 0);
}



