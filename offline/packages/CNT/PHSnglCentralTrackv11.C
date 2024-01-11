
#include "PHSnglCentralTrackv11.h"

ClassImp(PHSnglCentralTrackv11)

PHSnglCentralTrackv11::PHSnglCentralTrackv11()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv11::PHSnglCentralTrackv11(const PHSnglCentralTrackv11 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}










