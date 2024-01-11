#include "PhCglSnglv2.h"

ClassImp(PhCglSnglv2)

using namespace std;

PhCglSnglv2::PhCglSnglv2()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PhCglSnglv2::PhCglSnglv2(const PhCglSnglv2 &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}




