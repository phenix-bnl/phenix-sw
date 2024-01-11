#include "PHSnglCentralTrackv16.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv16)

using namespace std;

PHSnglCentralTrackv16::PHSnglCentralTrackv16()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv16::PHSnglCentralTrackv16(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv16::identify(ostream &os) const
{
  os << "PHSnglTrack v16" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
