// $Id: PdbEmcHLRatioVector.cc,v 1.1 2003/12/19 10:55:12 aphecetc Exp $
//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbEmcHLRatioVector
//
//  Author: Hugues Delagrange
//-----------------------------------------------------------------------------
#include "PdbEmcHLRatioVector.hh"

#include <iostream>
#include <iomanip>
#include <cassert>

using namespace std;

PdbEmcHLRatioVector::PdbEmcHLRatioVector()
{
  // Define dummy values for a start
  fAverage   = -1.0;
  fRMS       = -1.0;
  fIntercept =  0.0;
  fSlope     =  0.0;
}

//______________________________________________________________________________
PdbEmcHLRatioVector::~PdbEmcHLRatioVector()
{
}
//______________________________________________________________________________
// Retrieve the values to build the vector
void PdbEmcHLRatioVector::GetRatioVector(float & theAverage,   float & theRMS,
                                         float & theIntercept, float & theSlope)
{
   theAverage = fAverage;
   theRMS     = fRMS;
   theIntercept = fIntercept;
   theSlope     = fSlope;
}
//______________________________________________________________________________
// Set the values of the vector
void PdbEmcHLRatioVector::SetRatioVector(float   theAverage,   float   theRMS,
                                         float   theIntercept, float   theSlope)
{
  // No check is performed by this method for the validity
  // of the parameters. This should have been made before
  fAverage = theAverage;
  fRMS     = theRMS;
  fIntercept = theIntercept;
  fSlope     = theSlope;
}


//______________________________________________________________________________

void PdbEmcHLRatioVector::print() const
{
  cout << "<I> PdbEmcHLRatioVector : " 
       << " Average = " << fAverage  
       << " RMS = " << fRMS      
       << " Intercept = " << fIntercept 
       << " Slope = " << fSlope    << endl;
}
