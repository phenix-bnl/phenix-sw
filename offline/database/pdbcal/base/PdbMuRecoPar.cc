//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuRecoPar
//
//  Author: rjnewby
//-----------------------------------------------------------------------------
#include <PdbMuRecoPar.hh>
#include <cstring>
#include <iostream>

PdbMuRecoPar::PdbMuRecoPar()
: parval(0.0)
{
  strcpy(parname, "Unknown");
}

PdbMuRecoPar::~PdbMuRecoPar()
{
}

void PdbMuRecoPar::print() const
{
  std::cout << parname << " : " << parval << std::endl;
}

void PdbMuRecoPar::setParName(const char* newName)
{
  strncpy(parname,newName,50);
  if(strlen(newName)>49)
  {
    parname[49]='\0';
  }
  return;
}
