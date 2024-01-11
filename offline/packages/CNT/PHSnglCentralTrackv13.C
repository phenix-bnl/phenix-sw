#include "PHSnglCentralTrackv13.h"
#include <iostream>

ClassImp(PHSnglCentralTrackv13)

using namespace std;

PHSnglCentralTrackv13::PHSnglCentralTrackv13()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PHSnglCentralTrackv13::PHSnglCentralTrackv13(const PHSnglCentralTrack &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack *track) method
  Copy(track);
  return;
}

void
PHSnglCentralTrackv13::identify(ostream &os) const
{
  os << "PHSnglTrack v13" << endl;
  os << "quality : " << get_quality() << endl;
  return;
}
