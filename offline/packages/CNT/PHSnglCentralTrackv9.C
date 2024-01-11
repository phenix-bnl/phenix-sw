
#include "PHSnglCentralTrackv9.h"

ClassImp(PHSnglCentralTrackv9)

PHSnglCentralTrackv9::PHSnglCentralTrackv9()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv9::PHSnglCentralTrackv9(const PHSnglCentralTrackv9 &track)
{

  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}










