#include "PhCglSngl.h"

ClassImp(PhCglSngl)

using namespace std;

PhCglSngl::PhCglSngl()
{
  // this uses the generic Init() function in the PHSnglCentralTrack base class
  // This will initialize the values to whatever is deemed apropriate 
  Init();
  return;
}

PhCglSngl::PhCglSngl(const PhCglSngl &track)
{
  // this calls the generic Copy method in the PHSnglCentralTrack base class
  // If you add variables, please add them to the Copy(PHSnglCentralTrack &track) method
  Copy(track);
  return;
}




