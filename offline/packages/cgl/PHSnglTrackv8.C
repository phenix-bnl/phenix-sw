#include <PHSnglTrackv8.h>
#include <cmath>

ClassImp(PHSnglTrackv8)

using namespace std;

// v8 adds tofw.
PHSnglTrackv8::PHSnglTrackv8()
{
  for (short int i = 0;i < 3;i++)
    {
       projectionTofw[i] = NAN;
       directionTofw[i] = NAN;
    }
  tofwPathLength = NAN;
  return ;
}
