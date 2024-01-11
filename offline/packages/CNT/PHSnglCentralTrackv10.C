
#include "PHSnglCentralTrackv10.h"

ClassImp(PHSnglCentralTrackv10)

PHSnglCentralTrackv10::PHSnglCentralTrackv10()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv10::PHSnglCentralTrackv10(const PHSnglCentralTrackv10 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}










