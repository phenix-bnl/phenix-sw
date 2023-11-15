//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuiTriggerMLU
//
//  Author: rjnewby
//-----------------------------------------------------------------------------
#include "PdbMuiTriggerMLU.hh"

#include <iostream>

PdbMuiTriggerMLU::PdbMuiTriggerMLU()
: _MLUindex(0), _MLUword(0)
{
}

PdbMuiTriggerMLU::~PdbMuiTriggerMLU()
{
}

void PdbMuiTriggerMLU::print() const
{
  std::cout<<"MLUindex(" << _MLUindex << ") MLUword(" << _MLUword << ")" <<std::endl;
}
