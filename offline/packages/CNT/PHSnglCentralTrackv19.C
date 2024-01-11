#include "PHSnglCentralTrackv19.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv19)

using namespace std;

PHSnglCentralTrackv19::PHSnglCentralTrackv19()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv19::PHSnglCentralTrackv19(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv19::identify(ostream &os) const
{
  os << "PHSnglTrack v19" << endl;
  os << "stecid : " << get_stecid() << endl;
  return;
}
