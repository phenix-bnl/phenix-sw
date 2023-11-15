//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadHV
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadHV.hh"

#include <iostream>

PdbPadHV::PdbPadHV()
{
  zero();
}

void PdbPadHV::zero()
{
  pc=0;
  arm=0;

  for (int i=0; i<32; i++) 
    hvstatus[i] = true; // let's assume everything is ok
}

PdbPadHV::~PdbPadHV()
{
}

void PdbPadHV::print() const
{  
  std::cout << "\nPC: " << pc << std::endl;
  std::cout << "\nArm: " << arm << std::endl;
  std::cout << "\nThe following HV sector indices are ok: " << std::endl;
  for (int i=0; i<32; i++) {
    if (hvstatus[i]) std::cout << i << " ";
  }
  std::cout << "\nThe following HV sector indices are not ok: " << std::endl;
  for (int i=0; i<32; i++) {
    if (!hvstatus[i]) std::cout << i << " ";
  }
  std::cout << std::endl;
}

bool PdbPadHV::get_HVStatusSect(short si) const
{  
  if ( (si>=0) && (si<32) ) {
    return hvstatus[si];
  }
  else {
    std::cerr << "PdbPadHV::get_HVStatusSect -- improper index !" << std::endl;
    return false;
  }
}

bool PdbPadHV::set_HVStatusSect(short si, bool b)
{  
  if ( (si>=0) && (si<32) ) {
    hvstatus[si] = b;
    return true;
  }
  else { 
    std::cerr << "PdbPadHV::set_HVStatusSect -- improper index !" << std::endl;
    return false;
  }
}

bool PdbPadHV::set_HVStatusAll(bool b[32])
{  
  for (int i=0; i<32; i++) {
    hvstatus[i] = b[i];
  }
  return true;
}

short PdbPadHV::get_NumOkHVSect() const
{  
  short nok=0;
  for (int i=0; i<32; i++) {
    if (hvstatus[i]) nok++;
  }
  return nok;
}

short PdbPadHV::get_NumNotOkHVSect() const
{  
  short nnotok=0;
  for (int i=0; i<32; i++) {
    if (!hvstatus[i]) nnotok++;
  }
  return nnotok;
}
