//  Implementation of class PdbTecPedestal
//  Author: Cesar Luiz da Silva

#include <iostream>
#include "PdbTecPedestal.hh"

using namespace std;

PdbTecPedestal::PdbTecPedestal() : nDim(32)
{
   zero();
}

void 
PdbTecPedestal::zero()
{
  for (size_t i = 0; i < nDim; i++) 
    {
      ADCPedestal[i] = 0.0;
    }
}

PdbTecPedestal::~PdbTecPedestal()
{
}

float 
PdbTecPedestal::getParameter(size_t i) const
{
  // Returns the parameter value at a given index location, 0.0 if the index is out of range.
  // coverity: unsigned int i is by definition > 0
  //  if (i >= 0 && i < nDim)
  if (i < nDim)
    {
      return ADCPedestal[i];
    }
  
  return 0.0;
}

void 
PdbTecPedestal::setParameter(size_t i, float val)
{
  // coverity: unsigned int i is by definition > 0
  //  if (i >= 0 && i < nDim)
   if (i < nDim)
    {
      ADCPedestal[i] = val;
    }
   else
     {
      cout << "PdbTecPedestal::SetParameter - Index = " << i 
	   << " is out of range. [0.." << nDim-1 << "] is valid." 
	   << endl;
     }
}

void 
PdbTecPedestal::print() const
{
for (size_t i = 0; i < nDim; i++) 
    {
      cout << "ADC = " << i << "  Pedestal = " << ADCPedestal[i] << endl;
    }
}







