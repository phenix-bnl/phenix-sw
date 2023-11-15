//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbEmcWalkTof
//
//  Author: delagran
//-----------------------------------------------------------------------------
#include "PdbEmcWalkTof.hh"

#include <iostream>
#include <iomanip>
#include <cassert>

using namespace std;

//______________________________________________________________________________
PdbEmcWalkTof::PdbEmcWalkTof()
{ 
  // Define dummy values to start
  fValue1     = -1.0;
  fValue2     = -2.0;
}// End Ctor

//______________________________________________________________________________
PdbEmcWalkTof::~PdbEmcWalkTof()
{
}

//______________________________________________________________________________
// Retrieve the values
void PdbEmcWalkTof::GetWalkTofs(float & thevalue1, float & thevalue2)
{
   thevalue1 = fValue1;
   thevalue2 = fValue2;
   
}// End ofPdbEmcWalkTof::GetWalkTofs  

// Set the values
void PdbEmcWalkTof::SetWalkTofs(float thevalue1, float thevalue2)
{
   fValue1 = thevalue1;
   fValue2 = thevalue2;
   
}// End ofPdbEmcWalkTof::SetWalkTofs  

//______________________________________________________________________________

void PdbEmcWalkTof::print() const
{
  cout << "<I> PdbEmcWalkTofs " ;
  cout << " Value1 = " << fValue1 ;
  cout << " Value2 = " << fValue2 << endl;

}// End of print
