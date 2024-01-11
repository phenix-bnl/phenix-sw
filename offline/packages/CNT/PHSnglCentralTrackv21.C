#include "PHSnglCentralTrackv21.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv21)

using namespace std;

PHSnglCentralTrackv21::PHSnglCentralTrackv21()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv21::PHSnglCentralTrackv21(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv21::identify(ostream &os) const
{
  os << "PHSnglTrack v21" << endl;
  return;
}
