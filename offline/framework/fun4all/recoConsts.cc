#include "recoConsts.h"

using namespace std;

recoConsts* recoConsts::__instance = 0;

void 
recoConsts::set_TimeStamp(PHTimeStamp t)
{
  TimeStamp = t;
}

recoConsts::recoConsts()
{
  TimeStamp = 0; 
}

void 
recoConsts::Print() const
{
  // methods from PHFlag
  PrintCharFlags();
  PrintFloatFlags();
  PrintIntFlags();

  return;
}
