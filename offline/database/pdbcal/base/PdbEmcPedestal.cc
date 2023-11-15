//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/database/pdbcal/base/PdbEmcPedestal.cc,v 1.1 2003/12/19 10:56:06 aphecetc Exp $
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbEmcPedestal
//
//  Author: L. Aphecetche
//-----------------------------------------------------------------------------
#include "PdbEmcPedestal.hh"

#include <iostream>
#include <iomanip>
#include <cassert>
#include <climits> // to get INT_MIN

using namespace std;

PdbEmcPedestal::PdbEmcPedestal()
{
  int i ;

  for (i=0;i<64;i++) {
    fLow[i]=fHigh[i]=fTAC[i]=INT_MIN ; // start with meaningless values.
  }
}

PdbEmcPedestal::~PdbEmcPedestal()
{
}

void PdbEmcPedestal::GetValues(int amu_cell, 
				int& low, int& high, int& tac)
{
  if (amu_cell>=0 && amu_cell<64) {
    low = fLow[amu_cell] ;
    high = fHigh[amu_cell] ;
    tac = fTAC[amu_cell] ;
  }
  else {
    low = high = tac = INT_MIN ;
    cerr << "<E> PdbEmcPedestal::Set - amu_cell out of range" << endl ;
  }
}

void PdbEmcPedestal::Set(int amu_cell, int low, int high, int tac)
{
  if (amu_cell>=0 && amu_cell<64) {
    fLow[amu_cell] = low ;
    fHigh[amu_cell] = high ;
    fTAC[amu_cell] = tac ;  
  }
  else {
    cerr << "<E> PdbEmcPedestal::Set - amu_cell out of range" << endl ;
  }
}

void PdbEmcPedestal::print() const
{
  int i ;

  cout << "PdbEmcPedestal:" << endl ;
  cout << "-------------------------------" << endl ;
  cout << "AMU-LOWPP-HIGHPP-TAC" << endl ;
  cout << "-------------------------------" << endl ;
  for (i=0;i<64;i++) {
    cout << "#" << setw(2) << i << " " 
	 << setw(5) << fLow[i] << " "
	 << setw(5) << fHigh[i] << " " 
	 << setw(5) << fTAC[i] << endl ;
  }
  cout << endl ;
}
