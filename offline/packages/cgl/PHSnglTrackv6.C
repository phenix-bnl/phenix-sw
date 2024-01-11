#include <PHSnglTrackv6.h>
#include <cmath>

ClassImp(PHSnglTrackv6)

using namespace std;

PHSnglTrackv6::PHSnglTrackv6()
{
  for (short int i = 0;i < 3;i++)
    {
      for (int ilayer = 0; ilayer < SVXLAYERNUMBER; ilayer++)
        {
          projectionSvx[ilayer][i] = NAN;
          directionSvx[ilayer][i] = NAN;
        }
    }
  return ;
}
