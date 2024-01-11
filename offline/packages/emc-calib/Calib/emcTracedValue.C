// $Header
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 1999
//
// E.Kistenev         11/21/99 
// Modified 16-Dec-1999 by L. Aphecetche
//
// send comments to kistenev@bnl.gov and aphecetc@in2p3.fr
//-----------------------------------------------------------------------------


#include <iostream> 
#include <cstdio>
#include <Rtypes.h>
#include "emcTracedValue.h"
#include <cmath>
#include <cassert>

using namespace std;

//_____________________________________________________________________________
emcTracedValue::emcTracedValue(Float_t* BP, bool isconstant)
{
  // it is assumed that BP is of dimension 4.
  if (BP) {
    Set(static_cast<int>(floor(BP[0])),BP[3],BP[2],isconstant) ;
  } else {
    Set(0,0,0,isconstant) ;
  }
}

//_____________________________________________________________________________
emcTracedValue::emcTracedValue(int thetime, float constant, 
			       float slope, bool isconstant) 
{  
  Set(thetime,constant,slope,isconstant) ;
}

//_____________________________________________________________________________
emcTracedValue::emcTracedValue(const emcTracedValue& val)
{
  GLine[0] = val.GLine[0] ;
  GLine[1] = val.GLine[1] ;
  GLine[2] = val.GLine[2] ;
  fIsConstant = val.fIsConstant ;
}

//_____________________________________________________________________________
emcTracedValue&
emcTracedValue::operator=(const emcTracedValue& val)
{
  if ( this == &val ) return *this ;
  GLine[0] = val.GLine[0] ;
  GLine[1] = val.GLine[1] ;
  GLine[2] = val.GLine[2] ;
  fIsConstant = val.fIsConstant ;
  return *this ;
}

//_____________________________________________________________________________
Float_t emcTracedValue::getValue(Float_t t)
{
  if ( t >= GLine[0] && !fIsConstant ) {
    return GLine[1]+GLine[2]*(t-GLine[0]) ;
  }
  else {
    return GLine[1] ;
  }
}

//_____________________________________________________________________________
void emcTracedValue::Set(int thetime, float constant, 
			 float slope) 
{
  GLine[0] = thetime ;
  GLine[1] = constant ;
  GLine[2] = slope ;
}

//_____________________________________________________________________________
void emcTracedValue::Set(int thetime, float constant, 
			 float slope, bool isconstant) 
{
  Set(thetime,constant,slope) ;
  fIsConstant = isconstant ;
}


//_____________________________________________________________________________
void emcTracedValue::readData(FILE * fp)
{
  assert( fscanf(fp,"%f %f %f", &GLine[0],&GLine[1],&GLine[2]) == 3);
}

//_____________________________________________________________________________
void emcTracedValue::writeData(FILE * fp)
{
  fprintf(fp,"  %10.0f  %10.5f  %14.9f", GLine[0],GLine[1],GLine[2]);
}

//_____________________________________________________________________________
ostream& operator << (ostream& out, const emcTracedValue& val)
{
  out << ((int)val.GLine[0]) 
      << " " << val.GLine[1] << " " << val.GLine[2] ;
  if ( val.isConstant() ) out << " (constant) " ;
  out << endl ;
  return out ;  
}
 
