#include <TecOutV3.hh>
#include <TecShortHit.hh>
#include <TecTrackTRv1.hh>
#include <mTecUtilities.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(TecOutV3)

using namespace std;

static int TECMAXNUMHITS = 2000;
static int TECMAXNUMTRACKS = 10;


TecOutV3::TecOutV3() {
  NumberOfHits=0;
  NumberOfTracks=0;
  TecHits = new TClonesArray("TecShortHit",TECMAXNUMHITS);
  TecTracks = new TClonesArray("TecTrackTRv1",TECMAXNUMTRACKS);
  RunNumber=0;
}

TecOutV3::~TecOutV3() {
  Clear();
  delete TecHits;
  delete TecTracks;
}

void TecOutV3::Clear(Option_t *option) {
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

void TecOutV3::ClearHits(Option_t *option) {
  TecHits->Clear();
  if (TecHits->GetSize() > TECMAXNUMHITS)
    {
      TecHits->Expand(TECMAXNUMHITS);
    }
  NumberOfHits = 0;
}

void TecOutV3::ClearTracks(Option_t *option) {
  TecTracks->Clear();
  if (TecTracks->GetSize() > TECMAXNUMTRACKS)
    {
      TecTracks->Expand(TECMAXNUMTRACKS);
    }
  NumberOfTracks = 0;
}

void TecOutV3::Reset() {
  Clear();
}

int TecOutV3::AddTecHit(int iindex, int iwire, int ibin,
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

int TecOutV3::AddTecTrack(float* xyzin, float* xyzout) {
    
  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv1(xyzin, xyzout);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv1(xyzin, xyzout);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}

int TecOutV3::AddTecTrack(TecTrack &source) {

  if(NumberOfTracks < TecTracks->GetSize()) {
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv1(source);
    NumberOfTracks++;
  }
  else {
    int MaxNumberOfTracks = TecTracks->GetSize() + TECMAXNUMTRACKS;
    TecTracks->Expand(MaxNumberOfTracks);
    TClonesArray &tectracks = *TecTracks;
    new(tectracks[NumberOfTracks]) TecTrackTRv1(source);
    NumberOfTracks++;
  }

  return NumberOfTracks;
}


//=============================================================================

int TecOutV3::getHitTrackID(int ihit, int nn) const {
  if(nn<0 || nn>2) nn=0;
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid(nn);
}

void TecOutV3::setHitTrackID(int ihit, int nn, int trkid) {
  if(nn<0 || nn>2) nn=0;
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(nn,trkid);
}

int TecOutV3::getHitTrackID(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_trackid();
}

void TecOutV3::setHitTrackID(int ihit, int trkid) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_trackid(trkid);
}

int TecOutV3::getHitGlobalIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_globalindex();
}

int TecOutV3::getHitIndex(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index();
}

int TecOutV3::getHitSector(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()/(TECMAXPLANE*TECMAXSIDE);
}

int TecOutV3::getHitPlane(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  int ind = techit->get_index();
  int nss = TECMAXPLANE*TECMAXSIDE;
  return (ind - (ind/nss)*nss)/TECMAXSIDE;
}

int TecOutV3::getHitSide(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_index()%TECMAXSIDE;
}

int TecOutV3::getHitWire(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_wire();
}

int TecOutV3::getHitTimeBin(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_bin();
}

int TecOutV3::getHitADC(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_adc();
}

float TecOutV3::getHitCharge(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_charge();
}

void TecOutV3::setHitADC(int ihit, int adc) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_adc(adc);
}

void TecOutV3::setHitCharge(int ihit, float ch) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_charge(ch);
}

float TecOutV3::getHitX(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_x();
}

void TecOutV3::setHitX(int ihit, float xx) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_x(xx);
}

float TecOutV3::getHitY(int ihit) const {
  TecShortHit* techit = (TecShortHit*)GetTecHits()->UncheckedAt(ihit);
  return techit->get_y();
}

void TecOutV3::setHitY(int ihit, float yy) {
  ((TecShortHit*)GetTecHits()->UncheckedAt(ihit))->set_y(yy);
}

//=============================================================================

int TecOutV3::getTrackSector(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSector();
}

int TecOutV3::getTrackSide(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSide();
}

int TecOutV3::getTrackIndex(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIndex();
}

float TecOutV3::getTrackXin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXin();
}
float TecOutV3::getTrackXinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXinError();
}

float TecOutV3::getTrackYin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYin();
}
float TecOutV3::getTrackYinError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYinError();
}

float TecOutV3::getTrackXout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXout();
}
float TecOutV3::getTrackXoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getXoutError();
}

float TecOutV3::getTrackYout(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYout();
}
float TecOutV3::getTrackYoutError(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getYoutError();
}

int TecOutV3::getTrackNhits(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits();
}

int TecOutV3::getTrackNhits100(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits100();
}

int TecOutV3::getTrackNhits200(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits200();
}

int TecOutV3::getTrackNhits50(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNhits50();
}

int TecOutV3::getTrackNhits(int itrack, int plane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNHITS(plane);
}

int TecOutV3::getTrackNwires(int itrack, int iplane) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNwires(iplane);
}

float TecOutV3::getTrackdEdX(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX();
}

float TecOutV3::getTrackdEdX06(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getdEdX06();
}

float TecOutV3::getTrackdEdX(int itrack, int iplane) const {
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

float TecOutV3::getTrackLength(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getTrackLength();
}

int TecOutV3::getTrackNdEdXbins(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getNdEdXbins();
}

float TecOutV3::getTrackAlpha(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getAlpha();
}

float TecOutV3::getTrackPhi(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getPhi();
}

float TecOutV3::getTrackCharge(int itrack) const {
  cout << "TecOutV3::getTrackCharge WARNING: returning sign of Alpha angle!" << endl;
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  float alpha = tectrack->getAlpha();
  if(alpha>0) return 1.; else return -1.;
}

float TecOutV3::getTrackPt(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->gettecMomentum();
}

float TecOutV3::getTrackSlope(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getSlope();
}

float TecOutV3::getTrackIntercept(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getIntercept();
}

float TecOutV3::getTrackWeightedTimeBin(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBin();
}

float TecOutV3::getTrackWeightedTimeBinSq(int itrack) const {
  TecTrackTR* tectrack = (TecTrackTR*)GetTecTracks()->UncheckedAt(itrack);
  return tectrack->getWeightedTimeBinSq();
}

void TecOutV3::identify(ostream& out) const {
  out << "I am a TecOutV3 object." << endl;
  out << "Number of Hits: " << getNHits()
      << ", Number of Tracks: " << getNTracks() << endl;
  return;
}

void TecOutV3::setTrackIndex(int itrack, int ind) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSector(ind/2);
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setSide(ind%2);
  return;
}

void TecOutV3::setTrackLength(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setTrackLength(a);
  return;
}

void TecOutV3::setTrackNdEdXbins(int itrack, int a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNdEdXbins(a);
  return;
}

void TecOutV3::setTrackdEdX(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX(a);
  return;
}

void TecOutV3::setTrackdEdX06(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setdEdX06(a);
  return;
}

void TecOutV3::setTrackNwires(int itrack, int iplane, int nw) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNwires(iplane,nw);
  return;
}

void TecOutV3::setTrackNhits(int itrack, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits(nh);
  return;
}

void TecOutV3::setTrackNhits100(int itrack, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits100(nh);
  return;
}

void TecOutV3::setTrackNhits200(int itrack, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits200(nh);
  return;
}

void TecOutV3::setTrackNhits50(int itrack, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNhits50(nh);
  return;
}

void TecOutV3::setTrackNhits(int itrack, int iplane, int nh) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setNHITS(iplane,nh);
  return;
}

void TecOutV3::setTrackWeightedTimeBin(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBin(a);
  return;
}

void TecOutV3::setTrackWeightedTimeBinSq(int itrack, float a) {
  ((TecTrackTR*)GetTecTracks()->UncheckedAt(itrack))->setWeightedTimeBinSq(a);
  return;
}


//------------------------  microDST methods -------------------------

int TecOutV3::isValid() const {
  return((NumberOfTracks>0 || NumberOfHits>0) ? 1 : 0);
}

int TecOutV3::get_index(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getIndex() : -999);
}


void TecOutV3::set_index(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setIndex(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}


int TecOutV3::get_nhits(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getNhits() : -999);
}

void TecOutV3::set_nhits(const unsigned int itrk, const int ival) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setNhits(ival); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV3::get_pt(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->gettecMomentum() : -999);
}

void TecOutV3::set_pt(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV3::get_xin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXin() : -999);
}

void TecOutV3::set_xin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV3::get_xout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getXout() : -999);
}

void TecOutV3::set_xout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setXout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV3::get_yin(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYin() : -999);
}

void TecOutV3::set_yin(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYin(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV3::get_yout(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getYout() : -999);
}

void TecOutV3::set_yout(const unsigned int itrk, const float rval) {
  TecTrackTR *tectrk = (TecTrackTR*) GetTecTracks()->UncheckedAt(itrk);
  if (tectrk) { tectrk->setYout(rval); }
    else { cout << PHWHERE << "ERROR no TecTrack object found." << endl; }
  return;
}

float TecOutV3::get_phi(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPhi() : -999);
}

void TecOutV3::set_phi(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}

float TecOutV3::get_alpha(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getAlpha() : -999);
}

void TecOutV3::set_alpha(const unsigned int itrk, const float rval) {
  cout << PHWHERE << "This method is not available in DST." << endl;
  return;
}


float TecOutV3::get_dEdx1(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float trklen = tectrk->getTrackLength();
  float dE = tectrk->getdEdX();
    if(trklen>0) { return dE/trklen; }
      else { return -999.; }
}

void TecOutV3::set_dEdx1(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

float TecOutV3::get_dEdx2(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  float nbins = (float)tectrk->getNdEdXbins();
  float dE = tectrk->getdEdX();
    if(nbins>0) { return dE/nbins; }
      else { return -999.; }
}

void TecOutV3::set_dEdx2(const unsigned int itrk, const float rval) { 
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV3::get_Pc1Hit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV3(TecTrackTRv1)." << endl;
  return -1;
}

void TecOutV3::set_Pc1Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV3::get_Pc3Hit(const unsigned int itrk) const {
  TecTrackTR *tectrk = (TecTrackTR *) GetTecTracks()->UncheckedAt(itrk);
  return((tectrk) ? tectrk->getPc3Pointer() : -999);
}

void TecOutV3::set_Pc3Hit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV3::get_TofHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV3(TecTrackTRv1)." << endl;
  return -1;
}

void TecOutV3::set_TofHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

short int TecOutV3::get_EmcHit(const unsigned int itrk) const {
  cout << PHWHERE << "ERROR This method is not available in TecOutV3(TecTrackTRv1)." << endl;
  return -1;
}

void TecOutV3::set_EmcHit(const unsigned int itrk, const short int rval) {
  cout << PHWHERE << "ERROR This method is not available in DST." << endl;
  return;
}

int TecOutV3::getMaxNHits() const {
  return TecHits->GetSize();
}

int TecOutV3::getMaxNTracks() const {
  return TecTracks->GetSize();
}


