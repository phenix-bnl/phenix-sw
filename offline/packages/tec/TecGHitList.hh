#ifndef TECGHITLIST_h
#define TECGHITLIST_h

#include <iostream>
#include "TClonesArray.h"
#include "TecGHit.hh"
#include "PHObject.h"

const int TECGHITLISTEXPANDSIZE = 100;

/**
This class has a TClonesArray of TecGHit objects, which
relate Tec Hits with Geant information.
Detailed documentation: not ready yet.
@author Sasha Lebedev (ISU)
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
*/
class TecGHitList: public PHObject {

public:

///
  TecGHitList();
///
  virtual ~TecGHitList();

///
  void Reset();
///
  void Clear(Option_t *option = "");
///
  void identify(std::ostream& os = std::cout) const;

///
  int get_hitid(int ihit);
///
  int get_tecghitid(int ihit);
///
  int get_fkinid(int ihit);
///
  int get_tecghitid(int ihit, int i);
///
  int get_fkinid(int ihit, int i);
///
  float get_fraction(int ihit);
///
  float get_fraction(int ihit, int i);

///
  void set_hitid(int ihit, int a);
///
  void set_tecghitid(int ihit, int a);
///
  void set_fkinid(int ihit, int a);
///
  void set_tecghitid(int ihit, int i, int a);
///
  void set_fkinid(int ihit, int i, int a);
///
  void set_fraction(int ihit, float a);
///
  void set_fraction(int ihit, int i, float a);


///
  int AddTecGHit(int ihitid, int itecghitid, int ifkinid);
///
  int AddTecGHit(const TecGHit* tecghit);

///
  TClonesArray *GetTecGHits() const {return TecGHits;}
///
  int getNHits() {return TecGHits->GetEntries();}
///
  int getSize() {return TecGHits->GetSize();}


protected:

/// List of Tec GHits
  TClonesArray *TecGHits;

  ClassDef(TecGHitList,1)

};

#endif


