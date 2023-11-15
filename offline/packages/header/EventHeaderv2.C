#include "EventHeaderv2.h"

using namespace std;

ClassImp(EventHeaderv2)

EventHeaderv2::EventHeaderv2()
{
  Reset();
  return;
}

void 
EventHeaderv2::Reset()
{
  TimeStamp = 0;
  EvtSequence = -999999;
  EvtType = -999999;
  return;
}

void 
EventHeaderv2::identify(ostream& out) const
{
  out << "identify yourself: I am an EventHeaderv2 Object" << endl;
  out << "Event no: " << EvtSequence 
      << ", Type: " << EvtType 
      << ", ATP arrival time: " << ctime(&TimeStamp)
      << endl;

  return;
}

int 
EventHeaderv2::isValid() const
{
  return((TimeStamp) ? 1:0); // return 1 if TimeStamp is not zero
}
