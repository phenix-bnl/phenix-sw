#include "PHSnglCentralTrackv18.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv18)

using namespace std;

PHSnglCentralTrackv18::PHSnglCentralTrackv18()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv18::PHSnglCentralTrackv18(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv18::identify(ostream &os) const
{
  os << "PHSnglTrack v18" << endl;
  os << "stecid : " << get_stecid() << endl;
  return;
}
