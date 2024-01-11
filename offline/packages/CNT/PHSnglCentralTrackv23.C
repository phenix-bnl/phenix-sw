#include "PHSnglCentralTrackv23.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv23)

using namespace std;

PHSnglCentralTrackv23::PHSnglCentralTrackv23()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv23::PHSnglCentralTrackv23(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv23::identify(ostream &os) const
{
  os << "PHSnglTrack v23" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
