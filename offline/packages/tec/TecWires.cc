#include "TecWires.hh"

TecWires::TecWires() {
  MaxNumberOfWires=TECMAXNUMW;
  NumberOfWires=0;
  TecWireList = new TClonesArray("TecWire",MaxNumberOfWires);
}

TecWires::~TecWires() {
  Clear();
}

void TecWires::Clear(Option_t *option) {
  TecWireList->Clear();
  MaxNumberOfWires = TECMAXNUMW;
  NumberOfWires = 0;
}

void TecWires::Reset() {
  Clear();
}

int TecWires::AddTecWire(int iindex, int iwire, int* iadc, float* fcharge, float* fxyz) {

  if(NumberOfWires < MaxNumberOfWires) {
    TClonesArray &tecwires = *TecWireList;
    new(tecwires[NumberOfWires]) TecWire(iindex, iwire, iadc, fcharge, fxyz);
    NumberOfWires++;
  }
  else {
    MaxNumberOfWires += TECMAXNUMW;
    TecWireList->Expand(MaxNumberOfWires);
    TClonesArray &tecwires = *TecWireList;
    new(tecwires[NumberOfWires]) TecWire(iindex, iwire, iadc, fcharge, fxyz);
    NumberOfWires++;
  }

  return NumberOfWires;
}

int TecWires::AddTecWire(int iindex, int iwire) {

  int iadc[80]; for(int i=0; i<TECMAXTIMEBIN; i++) {iadc[i]=0; }
  float fcharge[80]; for(int i=0; i<TECMAXTIMEBIN; i++) {fcharge[i]=0.; }
  float fxyz[3]; for(int i=0; i<3; i++) {fxyz[i]=0.; }

  if(NumberOfWires < MaxNumberOfWires) {
    TClonesArray &tecwires = *TecWireList;
    new(tecwires[NumberOfWires]) TecWire(iindex, iwire, iadc, fcharge, fxyz);
    NumberOfWires++;
  }
  else {
    MaxNumberOfWires += TECMAXNUMW;
    TecWireList->Expand(MaxNumberOfWires);
    TClonesArray &tecwires = *TecWireList;
    new(tecwires[NumberOfWires]) TecWire(iindex, iwire, iadc, fcharge, fxyz);
    NumberOfWires++;
  }

  return NumberOfWires;
}

//==============================================================================

int TecWires::getIndex(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getIndex();
}

int TecWires::getWire(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getWire();
}

int* TecWires::getADC(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getADC();
}

int TecWires::getADC(int iwire, int ibin) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getADC(ibin);
}

float* TecWires::getCharge(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getCharge();
}

float TecWires::getCharge(int iwire, int ibin) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getCharge(ibin);
}

int* TecWires::getHitnumber(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getHitnumber();
}

int TecWires::getHitnumber(int iwire, int ibin) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getHitnumber(ibin);
}

float TecWires::getX(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getX();
}

float TecWires::getY(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getY();
}

float TecWires::getZ(int iwire) {
  TecWire* tecwire = (TecWire*)GetTecWires()->UncheckedAt(iwire);
  return tecwire->getZ();
}

void TecWires::setIndex(int iwire, int ind) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setIndex(ind);
}

void TecWires::setWire(int iwire, int ind) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setWire(ind);
}

void TecWires::setADC(int iwire, int ibin, int iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setADC(ibin, iv);
}

void TecWires::setADC(int iwire, int* iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setADC(iv);
}

void TecWires::setCharge(int iwire, int ibin, float iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setCharge(ibin, iv);
}

void TecWires::setCharge(int iwire, float* iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setCharge(iv);
}

void TecWires::setHitnumber(int iwire, int ibin, int iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setHitnumber(ibin, iv);
}

void TecWires::setHitnumber(int iwire, int* iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setHitnumber(iv);
}

void TecWires::setX(int iwire, float iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setX(iv);
}

void TecWires::setY(int iwire, float iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setY(iv);
}

void TecWires::setZ(int iwire, float iv) {
  ((TecWire*)GetTecWires()->UncheckedAt(iwire))->setZ(iv);
}



