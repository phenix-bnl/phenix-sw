//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuiTubeEff
//
//  Author: rjnewby
//-----------------------------------------------------------------------------
#include "PdbMuiTubeEff.hh"

#include <iostream>

//____________________________________________________________
void PdbMuiTubeEff::print() const
{ 
  std::cout 
    << arm() << " " 
    << plane() << " " 
    << panel() << " " 
    << orientation() << " " 
    << twopack() << " " 
    << eff() << " "
    << entries() 
    << std::endl; 
}
