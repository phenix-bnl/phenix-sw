#ifndef TECWIRES_H
#define TECWIRES_H

#include "TecWire.hh"
#include "TClonesArray.h"

#define TECMAXNUMW 100

/**
This class contains a list of fired Tec wires.
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
*/

class TecWires {

public:

///
  TecWires();
///
  virtual ~TecWires();
///
  void Reset();
///
  void Clear(Option_t *option = "");

///
  int getIndex(int iwire);
///
  int getWire(int iwire);
///
  int* getADC(int iwire);
///
  int getADC(int iwire, int ibin);
///
  float* getCharge(int iwire);
///
  float getCharge(int iwire, int ibin);
///
  int* getHitnumber(int iwire);
///
  int getHitnumber(int iwire, int ibin);
///
  float getX(int iwire);
///
  float getY(int iwire);
///
  float getZ(int iwire);

///
  void setIndex(int iwire, int a);
///
  void setWire(int iwire, int a);
///
  void setADC(int iwire, int* a);
///
  void setADC(int iwire, int ibin, int a);
///
  void setCharge(int iwire, float* a);
///
  void setCharge(int iwire, int ibin, float a);
///
  void setHitnumber(int iwire, int* a);
///
  void setHitnumber(int iwire, int ibin, int a);
///
  void setX(int iwire, float a);
///
  void setY(int iwire, float a);
///
  void setZ(int iwire, float a);

///
  int AddTecWire(int iindex, int iwire, int* iadc, float* fcharge, float* fxyz);
///
  int AddTecWire(int iindex, int iwire);
///
  TClonesArray *GetTecWires() const {return TecWireList;}
///
  int getNWires() {return NumberOfWires;}
///
  int getMaxNWires() {return MaxNumberOfWires;}

protected:

/// Current number of wires in the object
  int NumberOfWires;
///
  int MaxNumberOfWires;
/// List of Tec fired wires
  TClonesArray *TecWireList;

};

#endif


