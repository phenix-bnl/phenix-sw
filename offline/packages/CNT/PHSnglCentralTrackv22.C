#include "PHSnglCentralTrackv22.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv22)

using namespace std;

PHSnglCentralTrackv22::PHSnglCentralTrackv22()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv22::PHSnglCentralTrackv22(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv22::identify(ostream &os) const
{
  os << "PHSnglTrack v22" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
