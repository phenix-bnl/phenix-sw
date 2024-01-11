#include "PHDchTrackOut.h"

ClassImp(PHDchTrackOut)

using namespace std;

void PHDchTrackOut::Reset()
{
  cerr << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int PHDchTrackOut::isValid() const
{
  cerr << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void PHDchTrackOut::identify(ostream &os) const
{
  os << "identify yourself: virtual PHDchTrackOut object" << endl;
  return ;
}
