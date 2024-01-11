#include "PHSnglCentralTrackv12.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv12)

using namespace std;

PHSnglCentralTrackv12::PHSnglCentralTrackv12()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv12::PHSnglCentralTrackv12(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv12::identify(ostream &os) const
{
  os << "PHSnglTrack v12" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}







