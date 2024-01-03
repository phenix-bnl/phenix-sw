//-----------------------------------------------------------------------------
//
//  Call pair acceptance routines for different setups
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "InCERESAcceptance.h"
#include "Momentum.h"
#include "PairInAcceptance.h"

class Mom4;

bool PairInAcceptance(const int setup, const Mom4& mom1, const Mom4& mom2)
{
  bool accept = 0;

  switch(setup)
  {
    case 1:  accept = InCERESAcceptance(mom1,mom2);

             break;

    case 2:  accept = 0;

             break;

    case 3:  accept = 0;

             break;

    case 4:  accept = 0;

             break;

    case 5:  accept = 0;

             break;

    case 6:  accept = 0;

             break;

    case 7:  accept = 0;

             break;

    default: std::cerr << "Error: Setup " << setup << " not predefined"
             << std::endl;

             break;
  }

  return (accept);
}
