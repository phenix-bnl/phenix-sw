#include <PHSnglTrackv5.h>
#include <cmath>

ClassImp(PHSnglTrackv5)

using namespace std;


PHSnglTrackv5::PHSnglTrackv5()
{
  for (short int i=0;i<3;i++)
    {
      projectionMrpc[i] = NAN;
      directionMrpc[i] = NAN;
    }
  mrpcPathLength = NAN;
}
