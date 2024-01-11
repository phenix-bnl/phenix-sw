#include <PHSnglTrackv7.h>
#include <cmath>

ClassImp(PHSnglTrackv7)

using namespace std;

// v7 adds hbd.  Only put hbd-specific code here and inherit the rest from v4.
PHSnglTrackv7::PHSnglTrackv7()
{
  for (short int i = 0;i < 3;i++)
    {
       projectionHbd[i] = NAN;
       directionHbd[i] = NAN;
    }
  return ;
}
