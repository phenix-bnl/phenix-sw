//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbPadBadCh
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadBadCh.hh"

#include <iostream>
#include <iomanip>

PdbPadBadCh::PdbPadBadCh() : nDim(3)
{
  zero();
}

void PdbPadBadCh::zero()
{
  memset(BadChParameter,0,sizeof(BadChParameter));
}

int PdbPadBadCh::getParameter(const size_t i) const
{
   //
   // Returns the parameter value at a given index location, -1 if the index is out of range.
   //
  if (i < nDim)
    {
      return BadChParameter[i];
    }
  else
    {
      return -1;
    }

}

const char* PdbPadBadCh::getParName(const size_t i) const
{
   //
   // Returns the parameter name at a given index location. Returns -1 if the index is out of range.
   //
   switch(i) {
   case 0:
     return "Packetid"; // 4001 to 4096
   case 1:
     return "Channel index"; // 0 to 2159
   case 2:
     return "Padtype"; // 1=inactive,2=hot,3 etc for future classifications. 0 not used (reserved for ok pads)
   default:
      return "ERROR";
   }
}

void PdbPadBadCh::setParameter(const size_t i, const int val)
{
  if (i < nDim)
    {
      BadChParameter[i] = val;
    }
  else
    {
      std::cout << "PdbPadBadCh::SetParameter - Index = " << i  << " is out of range. [0.." << nDim - 1 << "] is valid." << std::endl;
        }
}


void PdbPadBadCh::print() const
{
   std::cout << "Packetid  = " << BadChParameter[0] << std::endl;
   std::cout << "Channel index  = " << BadChParameter[1] << std::endl;
   std::cout << "Padtype  = " << BadChParameter[2] << std::endl;
}
