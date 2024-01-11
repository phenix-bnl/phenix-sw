#ifndef EvtVector_HH
#define EvtVector_HH

#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <iostream.h>
#include <Rtypes.h>
#include <TObject.h>
#include <vector.h>
#include "Evt.hh"

/*
  This class is very virtual class and is not used.
  The purpose is to create dictionary of vector<Evt>.
  by using EvtVector_LinkDef.hh file
 */

class EvtVector : public TObject {
private:
  vector<Evt> _vec_evt;

public:
  EvtVector(){/*   */ };
  ~EvtVector(){/*    */};

  ClassDef(EvtVector,1)
};
//
#endif
//
