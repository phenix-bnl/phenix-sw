#include "PHSnglCentralTrackv17.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv17)

using namespace std;

PHSnglCentralTrackv17::PHSnglCentralTrackv17()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv17::PHSnglCentralTrackv17(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv17::identify(ostream &os) const
{
  os << "PHSnglTrack v17" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
