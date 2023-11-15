//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/database/pdbcal/base/PdbEmcPedestal5.cc,v 1.1 2003/12/19 10:55:48 aphecetc Exp $
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999-2001
//
//  Implementation of class PdbEmcPedestal5
//
//  Author: L. Aphecetche
//-----------------------------------------------------------------------------
#include "PdbEmcPedestal5.hh"

#include <iostream>
#include <iomanip>
#include <cassert>
#include <climits> // to get INT_MIN

using namespace std;

PdbEmcPedestal5::PdbEmcPedestal5()
{
  int i ;

  for (i=0;i<64;i++) {
    fHG_Pre[i]=fHG_Post[i]=fLG_Pre[i]=fLG_Post[i]=fTAC[i]=INT_MIN ; 
    // start with meaningless values.
  }
}

PdbEmcPedestal5::~PdbEmcPedestal5()
{
}

void PdbEmcPedestal5::GetValues(int amu_cell, int& hg_pre, int& hg_post,
				   int& lg_pre, int& lg_post, int& tac)
{
  if (amu_cell>=0 && amu_cell<64) {
    tac = fTAC[amu_cell] ;
    hg_pre = fHG_Pre[amu_cell] ;
    hg_post = fHG_Post[amu_cell] ;
    lg_pre = fLG_Pre[amu_cell] ;
    lg_post = fLG_Post[amu_cell] ;    
  }
  else {
    hg_pre = hg_post = lg_pre = lg_post = tac = INT_MIN ;
    cerr << "<E> PdbEmcPedestal5::Set - amu_cell out of range" << endl ;
  }
}

void PdbEmcPedestal5::Set(int amu_cell, int hg_pre, int hg_post, 
	   int lg_pre, int lg_post, int tac) 
{
  if (amu_cell>=0 && amu_cell<64) {
    fHG_Pre[amu_cell] = hg_pre ;
    fHG_Post[amu_cell] = hg_post ;
    fLG_Pre[amu_cell] = lg_pre ;
    fLG_Post[amu_cell] = lg_post ;    
    fTAC[amu_cell] = tac ;  
  }
  else {
    cerr << "<E> PdbEmcPedestal5::Set - amu_cell out of range" << endl ;
  }
}

void PdbEmcPedestal5::print() const
{
  int i ;

  cout << "PdbEmcPedestal5:" << endl ;
  cout << "-------------------------------" << endl ;
  cout << "AMU-HGPRE-HGPOST-LGPRE-LGPOST-TAC" << endl ;
  cout << "-------------------------------" << endl ;
  for (i=0;i<64;i++) {
    cout << "#" << setw(2) << i << " " 
	 << setw(5) << fHG_Pre[i] << " "
	 << setw(5) << fHG_Post[i] << " " 
	 << setw(5) << fLG_Pre[i] << " "
	 << setw(5) << fLG_Post[i] << " "       
	 << setw(5) << fTAC[i] << endl ;
  }
  cout << endl ;
}
