#include "PHSnglCentralTrackv14.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv14)

using namespace std;

PHSnglCentralTrackv14::PHSnglCentralTrackv14()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv14::PHSnglCentralTrackv14(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv14::identify(ostream &os) const
{
  os << "PHSnglTrack v14" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
