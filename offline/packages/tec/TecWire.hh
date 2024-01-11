#ifndef TECWIRE_H
#define TECWIRE_H

#include <iostream>
#include "PHObject.h"
#include "TecBasicObject.hh"

/** Represents one fired Tec wire */
class TecWire : public TObject {

 public:

/// Default constructor
  TecWire();
/// Constructor
  TecWire(int iindex, int iwire, int* iadc, float* fcharge, float* fxyz);
/// Constructor
  TecWire(int iindex, int iwire);
/// Destructor
  virtual ~TecWire() { }

///
 void identify(std::ostream& os = std::cout) const;

///
  int getIndex() { return index; }
///
  int getWire() { return wire; }
///
  int* getADC() { return adc; }
///
  int getADC(int i) { return adc[i]; }
///
  float* getCharge() { return charge; }
///
  float getCharge(int i) { return charge[i]; }
///
  int* getHitnumber() { return hitnumber; }
///
  int getHitnumber(int i) { return hitnumber[i]; }
///
  float getX() { return xyz[0]; }
///
  float getY() { return xyz[1]; }
///
  float getZ() { return xyz[2]; }
///
  float* getXYZ() { return xyz; }

///
  void setIndex(int a) { index = a; }
///
  void setWire(int a) { wire = a; }
///
  void setADC(int i, int a) { adc[i] = a; }
///
  void setADC(int* a) { for(int i=0; i<TECMAXTIMEBIN; i++) adc[i] = a[i]; }
///
  void setCharge(float* a) { for(int i=0; i<TECMAXTIMEBIN; i++) charge[i] = a[i]; }
///
  void setCharge(int i, float a) { charge[i] = a; }
///
  void setHitnumber(int* a) { for(int i=0; i<TECMAXTIMEBIN; i++) hitnumber[i] = a[i]; }
///
  void setHitnumber(int i, int a) { hitnumber[i] = a; }
///
  void setX(float a) { xyz[0] = a; }
///
  void setY(float a) { xyz[1] = a; }
///
  void setZ(float a) { xyz[2] = a; }

protected:

///
  int index;
///
  int wire;
///
  int adc[TECMAXTIMEBIN];
///
  float charge[TECMAXTIMEBIN];
///
  int hitnumber[TECMAXTIMEBIN];
///
  float xyz[3];

};

#endif // TECWIRE_H 

