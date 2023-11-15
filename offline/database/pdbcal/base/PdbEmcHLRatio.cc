//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999, 2000
//
//  Implementation of class PdbEmcHLRatio
//
//  Author: H. Delagrange
//-----------------------------------------------------------------------------
#include "PdbEmcHLRatio.hh"

#include <iostream>

PdbEmcHLRatio::PdbEmcHLRatio()
{
  fRatio = 0.0;
}

PdbEmcHLRatio::~PdbEmcHLRatio()
{
}

void PdbEmcHLRatio::print() const
{
}

//______________________________________________________________________
void PdbEmcHLRatio::SetRatio(const double dratio)
{ 
  if ( dratio > 0.0 ){
    fRatio = dratio;
  } //fi dratio
  else {
    fRatio = 0.0;
    std::cerr << "<E> PdbEmcHLRatio::SetRatio -- improper value !" << std::endl;
  }

}// End PdbEmcHLRatio::SetRatio(const double dratio)
