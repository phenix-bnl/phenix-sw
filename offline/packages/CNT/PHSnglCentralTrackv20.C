#include "PHSnglCentralTrackv20.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv20)

using namespace std;

PHSnglCentralTrackv20::PHSnglCentralTrackv20()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv20::PHSnglCentralTrackv20(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv20::identify(ostream &os) const
{
  os << "PHSnglCentralTrack v20" << endl;
  return;
}
