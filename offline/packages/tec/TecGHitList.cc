#include "TecGHitList.hh"
#include "TecGHit.hh"
#include "TClonesArray.h"
#include <iostream>

ClassImp(TecGHitList)

using namespace std;

TecGHitList::TecGHitList() {
  TecGHits = new TClonesArray("TecGHit",1);
}

TecGHitList::~TecGHitList() {
  Clear();
}

void TecGHitList::Clear(Option_t *option)
{
  TecGHits->Clear();
  if (TecGHits->GetSize() > 1)
    {
      TecGHits->Expand(1);
    }
}

void TecGHitList::Reset() {
  Clear();
}

int TecGHitList::AddTecGHit(const TecGHit* tecghit) {

  if(TecGHits->GetEntries()==TecGHits->GetSize()) {
    TecGHits->Expand(TecGHits->GetSize()+TECGHITLISTEXPANDSIZE);
  }
  TClonesArray &array = *TecGHits;
  new(array[TecGHits->GetEntries()]) TecGHit(tecghit);

  return TecGHits->GetEntries();

}

int TecGHitList::AddTecGHit(int ihitid, int itecghitid, int ifkinid) {

  if(TecGHits->GetEntries()==TecGHits->GetSize()) {
    TecGHits->Expand(TecGHits->GetSize()+TECGHITLISTEXPANDSIZE);
  }
  TClonesArray &array = *TecGHits;
  new(array[TecGHits->GetEntries()]) TecGHit(ihitid, itecghitid, ifkinid);

  return TecGHits->GetEntries();

}

//=============================================================================

int TecGHitList::get_hitid(int ihit) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_hitid();
}

void TecGHitList::set_hitid(int ihit, int ii) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_hitid(ii);
}

int TecGHitList::get_tecghitid(int ihit) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_tecghitid();
}

int TecGHitList::get_tecghitid(int ihit, int i) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_tecghitid(i);
}

void TecGHitList::set_tecghitid(int ihit, int a) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_tecghitid(a);
}

void TecGHitList::set_tecghitid(int ihit, int i, int a) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_tecghitid(i,a);
}

int TecGHitList::get_fkinid(int ihit) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_fkinid();
}

int TecGHitList::get_fkinid(int ihit, int i) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_fkinid(i);
}

void TecGHitList::set_fkinid(int ihit, int ii) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_fkinid(ii);
}

void TecGHitList::set_fkinid(int ihit, int i, int a) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_fkinid(i,a);
}

float TecGHitList::get_fraction(int ihit) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_fraction();
}

float TecGHitList::get_fraction(int ihit, int i) {
  TecGHit* tecghit = (TecGHit*)GetTecGHits()->UncheckedAt(ihit);
  return tecghit->get_fraction(i);
}

void TecGHitList::set_fraction(int ihit, float a) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_fraction(a);
}

void TecGHitList::set_fraction(int ihit, int i, float a) {
  ((TecGHit*)GetTecGHits()->UncheckedAt(ihit))->set_fraction(i,a);
}

void TecGHitList::identify(ostream& out) const {
  out << "I am a TecGHitList object." << endl;
}



