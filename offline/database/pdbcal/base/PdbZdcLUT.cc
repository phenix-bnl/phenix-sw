//  Implementation of class PdbZdcLUT
//  Author: chiu

#include "PdbZdcLUT.hh"

#include <iostream>
#include <iomanip>
using namespace std;

PdbZdcLUT::PdbZdcLUT()
{
  zero();
}

PdbZdcLUT::~PdbZdcLUT()
{
}

void
PdbZdcLUT::zero()
{
  for (int i = 0; i < PDBZDCLUTSIZE; i++)
    {
      lut[i] = 0.0;
    }
}

void PdbZdcLUT::print() const
{
  cout.setf(ios::scientific, ios::floatfield);
  for (int i = 0; i < PDBZDCLUTSIZE; i++)
    {
      std::cout << lut[i];
      if (i % 8 == 7)
	{
	  std::cout << std::endl;
	}
      else
        {
          std::cout << "\t";
        }
    }
}
