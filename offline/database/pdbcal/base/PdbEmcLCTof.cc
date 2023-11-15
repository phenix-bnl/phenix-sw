// $Id: PdbEmcLCTof.cc,v 1.1 2003/12/19 10:55:30 aphecetc Exp $
//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/database/pdbcal/base/PdbEmcLCTof.cc,v 1.1 2003/12/19 10:55:30 aphecetc Exp $
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999, 2000
//
//  Implementation of class PdbEmcLCTof
//
//  Author: Hugues Delagrange 
//-----------------------------------------------------------------------------
#include "PdbEmcLCTof.hh"

#include <iostream>
#include <iomanip>
#include <cassert>

using namespace std;

//______________________________________________________________________________
PdbEmcLCTof::PdbEmcLCTof()
{
  // Define dummy values to start
  fValue1     = -1.0;
  fValue2     = -2.0;
}// End Ctor

//______________________________________________________________________________
PdbEmcLCTof::~PdbEmcLCTof()
{
}

//______________________________________________________________________________
// Retrieve the values
void PdbEmcLCTof::GetLCTofs(float & thevalue1, float & thevalue2)
{
   thevalue1 = fValue1;
   thevalue2 = fValue2;
   
}// End ofPdbEmcLCTof::GetLCTofs  

// Set the values
void PdbEmcLCTof::SetLCTofs(float thevalue1, float thevalue2)
{
   fValue1 = thevalue1;
   fValue2 = thevalue2;
   
}// End ofPdbEmcLCTof::SetLCTofs  

//______________________________________________________________________________
void PdbEmcLCTof::print() const
{
  cout << "<I> PdbEmcLCTofs :" ;
  cout << " Value1 = " << fValue1  ;
  cout << " Value2 = " << fValue2  << endl ;
}// End of Print
