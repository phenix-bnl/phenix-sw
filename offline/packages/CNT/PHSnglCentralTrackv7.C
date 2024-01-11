
#include "PHSnglCentralTrackv7.h"

ClassImp(PHSnglCentralTrackv7)

PHSnglCentralTrackv7::PHSnglCentralTrackv7()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv7::PHSnglCentralTrackv7(const PHSnglCentralTrackv7 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);

  return;
}










