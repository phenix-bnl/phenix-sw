//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbPadBadROC
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadBadROC.hh"

#include <iostream>
#include <iomanip>

PdbPadBadROC::PdbPadBadROC() : nDim(4)
{
  zero();
}

void PdbPadBadROC::zero()
{
  memset(BadROCParameter,0,sizeof(BadROCParameter));
}

int PdbPadBadROC::getParameter(const size_t i) const
{
   //
   // Returns the parameter value at a given index location, -1 if the index is out of range.
   //
  if (i < nDim)
    {
      return BadROCParameter[i];
    }
  else
    {
      return -1;
    }

}

const char* PdbPadBadROC::getParName(const size_t i) const
{
   //
   // Returns the parameter name at a given index location. Returns -1 if the index is out of range.
   //
   switch(i) {
   case 0:
     return "Packetid"; // 4001 to 4096
   case 1:
     return "Group row"; // 0 to 4
   case 2:
     return "Group column"; // 0 to 8
   case 3:
     return "ROC type"; // 1 <=> all channels always off
     // 2 <=> all channels always on, 3 etc. for future classifications   
   default:
      return "ERROR";
   }
}

void PdbPadBadROC::setParameter(const size_t i, const int val)

{
  if (i < nDim)
    {
      BadROCParameter[i] = val;
    }
  else
    {
      std::cout << "PdbPadBadROC::SetParameter - Index = " << i  << " is out of range. [0.." << nDim - 1 << "] is valid." << std::endl;
    }
}


void PdbPadBadROC::print() const
{
   std::cout << "Packetid  = " << BadROCParameter[0] << std::endl;
   std::cout << "Group row  = " << BadROCParameter[1] << std::endl;
   std::cout << "Group column = " << BadROCParameter[2] << std::endl;
   std::cout << "ROC type = " << BadROCParameter[3] << std::endl;
}


