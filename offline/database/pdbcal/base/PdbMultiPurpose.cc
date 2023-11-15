// Author: Federica Ceretto

#include "PdbMultiPurpose.hh"
#include <iostream>
#include <iomanip.h>

PdbMultiPurpose::PdbMultiPurpose() : PdbParChan1F() {
  for (int ix = 0; ix < fNdim; ix++) {
    fValue[ix] = 0.0;
    fValueError[ix] = 0.0;
  }
}

PdbMultiPurpose::PdbMultiPurpose(const PdbMultiPurpose &rhs) 
{
  for (int ix = 0; ix < fNdim; ix++) {
    fValue[ix] = rhs.fValue[ix];
    fValueError[ix] = rhs.fValueError[ix];
  }

}

PdbMultiPurpose::PdbMultiPurpose(const unsigned int aChannelID) : PdbParChan1F(aChannelID)
{
   //
   // Same as default but pass the channelID to the PdbParChan1F ctor.
   //
   for (int ix = 0; ix < fNdim; ix++) {
      fValue[ix] = 0.0;
      fValueError[ix] = 0.0;
   }
}

PdbMultiPurpose::~PdbMultiPurpose() {}

size_t PdbMultiPurpose::GetNdim() const
{
  return fNdim;
}

float PdbMultiPurpose::GetParameter(size_t index) const
{
   //
   // Returns the parameter value at a given index location. Returns 0.0 if the index is out of range.
   //
   if (index >= 0 && index < fNdim)
      return fValue[index];
   else
      return 0.0;
}



float PdbMultiPurpose::GetParError(size_t index) const
{
   //
   // Returns the parameter error at a given index location. Returns 0.0 if the index is out of range.
   //
   if (index >= 0 && index < fNdim)
      return fValueError[index];
   else
      return 0.0;
}

const char* PdbMultiPurpose::GetParName(size_t index) const
{
   //
   // Returns the index name at a given index location. Returns (pointer) 0 if the index is out of range.
   //
   switch(index) {
   case a:
      return "a";
   case b:
      return "b";
   case c:
      return "c";
   case d:
      return "d";
   case e:
      return "e";
   default:
      return 0;
   }
}

void PdbMultiPurpose::SetParameter(size_t index, float value)
{
   if (index >= 0 && index < fNdim)
      fValue[index]=value;
   else {
    std::cout << "PdbMultiPurpose::SetParameter - Index value = " 
	 << index  << " is out of range. [0.."
	 << fNdim-1 << "] is valid." << std::endl;
   }
}

void PdbMultiPurpose::SetParError(size_t index, float value)
{
   if (index >= 0 && index < fNdim)
      fValueError[index]=value;
   else {
    std::cout << "PdbMultiPurpose::SetParError - Index value = " 
	 << index  << " is out of range. [0.."
	 << fNdim-1 << "] is valid." << std::endl;
   }
}

void PdbMultiPurpose::Print() const
{ 
  for (int i =0; i<fNdim; i++)
    std::cout << GetParName(i) << "\t\t" << fValue[i] << "\t+/-\t" << fValueError[i] << std::endl;
}


void PdbMultiPurpose::Copy(PdbParChan1F* achan)
{
  for (int ix = 0; ix < fNdim; ix++) {
    fValue[ix] = achan->GetParameter(ix);
    fValueError[ix]  = achan->GetParError(ix);
  }
}







