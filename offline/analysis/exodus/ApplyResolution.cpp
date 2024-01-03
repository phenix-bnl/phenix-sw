//-----------------------------------------------------------------------------
//
//  Call resolution routines for different setups
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "ApplyCERESResolution.h"
#include "ApplyPHENIXResolution.h"
#include "ApplyResolution.h"

class Mom4;

void ApplyResolution(const int setup, Mom4& mom4)
{
  switch(setup)
  {
    case 1:  ApplyCERESResolution(mom4);
             break;

    case 2:  break;

    case 3:  ApplyPHENIXResolution(mom4);
             break;

    case 4:  ApplyPHENIXResolution(mom4);
             break;

    case 5:  ApplyPHENIXResolution(mom4);
             break;

    case 6:  ApplyPHENIXResolution(mom4);
             break;

    case 7:  ApplyPHENIXResolution(mom4);
             break;

    default: std::cerr << "Error: Setup " << setup
             << " not predefined" << std::endl;
             break;
  }
}
