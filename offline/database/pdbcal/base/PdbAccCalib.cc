//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbAccCalib
//
//  Author: masui
//-----------------------------------------------------------------------------
#include "PdbAccCalib.hh"

PdbAccCalib::PdbAccCalib()
{
  status = 0;
  for(int i=0;i<npar;i++){
    calibpar[i] = 0.;
  }
}

PdbAccCalib::~PdbAccCalib()
{
}

void PdbAccCalib::print() const
{

  return;
}
