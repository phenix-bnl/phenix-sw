#ifndef __BBC_HH__
#define __BBC_HH__

#include "PHTimeStamp.h"

// Author: Takeshi Kohama

namespace Bbc
{
  //  If you want to add any elements into these enum, all functions
  //  which have these enum as arguments, should be checked how they
  //  act with the new elements.  

  // To specify BBC arm (South North)
  enum ArmType  {South, North};
  
};

/// typedef. eventually should be PHTimeStamp
typedef PHTimeStamp BbcTime_t;

/// define the number of PMTs on BBC
const int BBC_N_PMT = 128;

#endif /* __BBC_HH__ */
