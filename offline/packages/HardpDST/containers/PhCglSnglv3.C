#include "PhCglSnglv3.h"

ClassImp(PhCglSnglv3)

using namespace std;

PhCglSnglv3::PhCglSnglv3()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PhCglSnglv3::PhCglSnglv3(const PhCglSnglv3 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}




