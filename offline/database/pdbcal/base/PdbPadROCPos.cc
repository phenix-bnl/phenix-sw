//  Implementation of class PdbPadROCPos
//  Author: David Silvermyr

#include "PdbPadROCPos.hh"

#include <iostream>

using namespace std;

PdbPadROCPos::PdbPadROCPos() : nDim(4)
{
  zero();
}

void 
PdbPadROCPos::zero()
{
  memset(ROCPosParameter,0,sizeof(ROCPosParameter));
}

int 
PdbPadROCPos::getParameter(const size_t i) const
{
  // Returns the parameter value at a given index location, -1 if the
  // index is out of range.
   if (i<nDim)
     {
       return ROCPosParameter[i];
     }
   
   return -1;
}

const char* 
PdbPadROCPos::getParName(const size_t i) const
{
   // Returns the parameter name at a given index location. Returns -1
   // if the index is out of range.
   switch(i) 
     {
     case 0:
       return "Packetid"; // 4001 to 4096
     case 1:
       return "Group row"; // 0 to 4
     case 2:
       return "Group column"; // 0 to 8
     case 3:
       return "ROC number"; 
     default:
       return "ERROR";
     }
}

void 
PdbPadROCPos::setParameter(const size_t i, const int val)
{
  if (i < nDim)
    {
      ROCPosParameter[i] = val;
    }
  else
    {
      cout << "PdbPadROCPos::SetParameter - Index = " << i  << " is out of range. [0.." << nDim - 1 << "] is valid." << endl;
    }

}

void PdbPadROCPos::print() const
{
   cout << "Packetid  = " << ROCPosParameter[0] << endl;
   cout << "Group row  = " << ROCPosParameter[1] << endl;
   cout << "Group column = " << ROCPosParameter[2] << endl;
   cout << "ROC number = " << ROCPosParameter[3] << endl;
}


