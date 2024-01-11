
#include "PHSnglCentralTrackv6.h"

ClassImp(PHSnglCentralTrackv6)

PHSnglCentralTrackv6::PHSnglCentralTrackv6()
{
  // this uses the generic Copy function in the PHSnglCentralTrack base class
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv6::PHSnglCentralTrackv6(const PHSnglCentralTrackv6 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);

  return;
}










