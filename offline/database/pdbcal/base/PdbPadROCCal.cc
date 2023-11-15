//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbPadROCCal
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadROCCal.hh"

#include <iostream>
#include <iomanip>

PdbPadROCCal::PdbPadROCCal() : nDim(11)
{
   zero();
}

void PdbPadROCCal::zero()
{
  memset(ROCCalParameter,0,sizeof(ROCCalParameter));
}

int PdbPadROCCal::getParameter(const size_t i) const
{
   //
   // Returns the parameter value at a given index location, -1 if the index is out of range.
   //
  if (i < nDim)
    {
      return ROCCalParameter[i];
    }
  else
    {
      return -1;
    }
}


const char* PdbPadROCCal::getParName(const size_t i) const
{
   //
   // Returns the parameter name at a given index location. Returns -1 if the index is out of range.
   //
   switch(i) {
   case 0:
     return "ROC number"; 
   case 1:
     return "Measurement index"; // 1=DS@BNL,2=HAG@LU,3-6=ES/TA/AO@LU etc
   case 2:
     return "TGL1 MUX(TP1)"; // Calibration value: MUX amplitude as a function of TestPulse (TP1,2,3=5,15,30 (effectively))
   case 3:
     return "TGL1 MUX(TP2)"; 
   case 4:
     return "TGL1 MUX(TP3)";
   case 5:
     return "TGL2 MUX(TP1)"; 
   case 6:
     return "TGL2 MUX(TP2)"; 
   case 7:
     return "TGL2 MUX(TP3)";  
   case 8:
     return "TGL3 MUX(TP1)"; 
   case 9:
     return "TGL3 MUX(TP2)"; 
   case 10:
     return "TGL3 MUX(TP3)";  
   default:
      return "ERROR";
   }
}

void PdbPadROCCal::setParameter(const size_t i, const int val)
{
  if (i < nDim)
    {
      ROCCalParameter[i] = val;
    }
  else
    {
      std::cout << "PdbPadROCCal::SetParameter - Index = " << i  << " is out of range. [0.." << nDim - 1 << "] is valid." << std::endl;
        }
}



void PdbPadROCCal::print() const
{
   std::cout << "ROC number  = " << ROCCalParameter[0] << std::endl;
   std::cout << "Measurement index  = " << ROCCalParameter[1] << std::endl;
   std::cout << "TGL1 MUX(TP1)  = " << ROCCalParameter[2] << std::endl;
   std::cout << "TGL1 MUX(TP2)  = " << ROCCalParameter[3] << std::endl;
   std::cout << "TGL1 MUX(TP3)  = " << ROCCalParameter[4] << std::endl;
   std::cout << "TGL2 MUX(TP1)  = " << ROCCalParameter[5] << std::endl;
   std::cout << "TGL2 MUX(TP2)  = " << ROCCalParameter[6] << std::endl;
   std::cout << "TGL2 MUX(TP3)  = " << ROCCalParameter[7] << std::endl;
   std::cout << "TGL3 MUX(TP1)  = " << ROCCalParameter[8] << std::endl;
   std::cout << "TGL3 MUX(TP2)  = " << ROCCalParameter[9] << std::endl;
   std::cout << "TGL3 MUX(TP3)  = " << ROCCalParameter[10] << std::endl;
}
